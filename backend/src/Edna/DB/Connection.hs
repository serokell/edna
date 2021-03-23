module Edna.DB.Connection
  ( ConnPool (..)
  , withPostgresConn
  , createConnPool
  , destroyConnPool
  ) where

import Universum

import Data.Pool (Pool, createPool, destroyAllResources)
import Data.Time.Clock (nominalDay)
import Database.Beam.Postgres (Connection, close, connectPostgreSQL)

import Edna.Config.Definition (EdnaConfig, dbConnString, dbMaxConnections, ecDb)
import Edna.Util (ConnString(..))

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
  (connectPostgreSQL connStr)   -- connection creation action
  close                         -- connection destroy action
  1                             -- number of individual pools (just one is fine)
  nominalDay                    -- maximum time the connection should remain open while unused
  maxConnsNum                   -- maximum number of DB connections.

-- | Destroys a @ConnPool@.
destroyConnPool :: MonadIO m => ConnPool -> m ()
destroyConnPool (ConnPool pool) = liftIO $ destroyAllResources pool
