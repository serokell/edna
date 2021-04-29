-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Edna.DB.Connection
  ( PostgresConn(..)
  , createConnPool
  , destroyConnPool
  , postgresConnPooled
  , postgresConnSingle
  , withPostgresConn
  ) where

import Universum

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (catchIOError)
import Data.Pool (Pool, createPool, destroyAllResources, withResource)
import Data.Time.Clock (nominalDay)
import Database.Beam.Postgres (Connection, close, connectPostgreSQL)
import Database.PostgreSQL.Simple.Transaction (withSavepoint, withTransactionSerializable)
import RIO (MonadUnliftIO, withRunInIO)

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

-- | Provides methods for working with connections to Postgres database.
data PostgresConn = PostgresConn
  { pcWithConnection  :: forall (m :: Type -> Type) a. MonadUnliftIO m
                      => (Connection -> m a) -> m a
  , pcWithTransaction :: forall (m :: Type -> Type) a. MonadUnliftIO m
                      => Connection -> m a  -> m a
  }

-- | Implementation of 'PostgresConn' which uses a 'ConnPool' to support
-- multithreading. Methods have the following properties:
--
-- * 'pcWithConnection' blocks if no idle connections are available and the
--   maximum number of DB connections has been reached.
-- * 'pcWithTransaction' uses @withTransactionSerializable@ as implementation.
postgresConnPooled :: ConnPool -> PostgresConn
postgresConnPooled (ConnPool pool) = PostgresConn
  { pcWithConnection  = \action ->
      withRunInIO $ \unlift ->
        withResource pool $ unlift . action
  , pcWithTransaction = \conn action ->
      withRunInIO $ \unlift ->
        withTransactionSerializable conn $ unlift action
  }

-- | Implementation of 'PostgresConn' which simply uses the single connection.
-- It's only intended to be constructed inside @transact@ in order to make sure
-- that all database actions wrapped with @transact@ are performed with a
-- single connection.
--
-- and 'pcWithTransaction' implementation is @withSavepoint@ to support
-- "nested" transactions
postgresConnSingle :: Connection -> PostgresConn
postgresConnSingle conn = PostgresConn
  { pcWithConnection  = ($ conn)
  , pcWithTransaction = \conn' action ->
      withRunInIO $ \unlift ->
        withSavepoint conn' $ unlift action
  }
