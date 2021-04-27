-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Edna.Setup
  ( Edna
  , dumpConfig
  , runEdna
  , fromConfig

  , EdnaContext (..)
  , edConfig
  , edConnectionPool
  , edDebugDB
  ) where

import Universum

import qualified Data.ByteString as B
import qualified Data.Yaml as Y
import Lens.Micro.Platform (Getting, makeLenses)
import RIO (RIO, runRIO)

import Edna.Config.Definition (EdnaConfig)
import Edna.Config.Environment (askDebugDB)
import Edna.DB.Connection (ConnPool, withPostgresConn)

data EdnaContext = EdnaContext
  { _edConfig :: !EdnaConfig
  , _edConnectionPool :: !ConnPool
  , _edDebugDB :: !Bool
  }

makeLenses ''EdnaContext

type Edna = RIO EdnaContext

fromConfig :: Getting a EdnaConfig a -> Edna a
fromConfig getter = view (edConfig . getter)

-- | Create 'EdnaContext' and run 'Edna' action with this context.
runEdna :: EdnaConfig -> Edna a -> IO a
runEdna config action = do
  withPostgresConn config $ \pool -> do
    debugDb <- askDebugDB
    let ednaContext = EdnaContext
          { _edConfig = config
          , _edConnectionPool = pool
          , _edDebugDB = debugDb
          }
    runRIO ednaContext action

-- | Dump config to stdout
dumpConfig :: EdnaConfig -> IO ()
dumpConfig = B.putStr . Y.encode
