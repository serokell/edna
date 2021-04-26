-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Edna.DB.Connection
  ( ConnPool (..)
  , withPostgresConn
  , createConnPool
  , destroyConnPool
  ) where

import Universum

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (catchIOError)
import Data.Pool (Pool, createPool, destroyAllResources)
import Data.Time.Clock (nominalDay)
import Database.Beam.Postgres (Connection, close, connectPostgreSQL)

import Edna.Config.Definition (EdnaConfig, dbConnString, dbMaxConnections, ecDb)
import Edna.Util (ConnString(..), logUnconditionally)

-- | Database connection pool. One @Connection@ can not be used simultaneously
-- by multiple threads, so we need a pool of connections to allow for
-- simultaneous DB queries.
newtype ConnPool = ConnPool
  { unConnPool :: Pool Connection
  } deriving stock (Show)

withPostgresConn :: (MonadIO m, MonadMask m) => EdnaConfig -> (ConnPool -> m a) -> m a
withPostgresConn config action = do
  let connString = config ^. ecDb . dbConnString
  let maxConnsNum = config ^. ecDb . dbMaxConnections
  bracket (createConnPool connString maxConnsNum) destroyConnPool action

-- | Creates a @ConnPool@ with PostgreSQL connections which use given
-- connection string.
createConnPool
  :: MonadIO m
  => ConnString
  -> Int
  -> m ConnPool
createConnPool (ConnString connStr) maxConnsNum = liftIO $ ConnPool <$>
  createPool
  (withRetry 8 $ connectPostgreSQL connStr)  -- connection creation action
  close        -- connection destroy action
  1            -- number of individual pools (just one is fine)
  nominalDay   -- maximum time the connection should remain open while unused
  maxConnsNum  -- maximum number of DB connections.
  where
    aSecond = 1e6
    -- We try to reconnect in case of an error because if we launch postgres
    -- and edna-server at nearly the same time, edna-server may try to connect
    -- before postgres starts accepting connections.
    -- It may happen if you deploy using @docker-compose@ for example.
    withRetry :: Word -> IO a -> IO a
    withRetry n action = action `catchIOError` \e -> do
      when (n == 0) $ throwM e
      logUnconditionally "Trying to connect to the DB…"
      threadDelay aSecond
      withRetry (n - 1) action

-- | Destroys a @ConnPool@.
destroyConnPool :: MonadIO m => ConnPool -> m ()
destroyConnPool (ConnPool pool) = liftIO $ destroyAllResources pool
