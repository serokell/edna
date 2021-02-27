module Edna.Setup
  ( Edna
  , runEdna

  , EdnaContext (..)
  , edConfig
  , edConnectionPool
  ) where

import Universum

import Lens.Micro.Platform (makeLenses)
import RIO (RIO, runRIO)

import Edna.Config.Definition (EdnaConfig(..))
import Edna.DB.Connection (ConnPool(..), withPostgresConn)

data EdnaContext = EdnaContext
  { _edConfig :: !EdnaConfig
  , _edConnectionPool :: !ConnPool
  }

makeLenses ''EdnaContext

type Edna = RIO EdnaContext

runEdna :: EdnaConfig -> Edna a -> IO a
runEdna config action = do
  withPostgresConn config $ \pool -> do
    let ednaContext = EdnaContext
          { _edConfig = config
          , _edConnectionPool = pool
          }
    runRIO ednaContext action
