-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Edna.Setup
  ( Edna
  , dumpConfig
  , fromConfig
  , runEdna

  , EdnaContext (..)
  , edConfig
  , edDBConnection
  , edDebugDB
  ) where

import Universum

import Control.Lens (Getting, makeLenses)
import qualified Data.ByteString as B
import qualified Data.Yaml as Y
import RIO (RIO, runRIO)

import Edna.Config.Definition (EdnaConfig)
import Edna.Config.Environment (askDebugDB)
import Edna.DB.Connection (PostgresConn, postgresConnPooled, withPostgresConn)

data EdnaContext = EdnaContext
  { _edConfig :: !EdnaConfig
  , _edDBConnection :: !PostgresConn
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
          , _edDBConnection = postgresConnPooled pool
          , _edDebugDB = debugDb
          }
    runRIO ednaContext action

-- | Dump config to stdout
dumpConfig :: EdnaConfig -> IO ()
dumpConfig = B.putStr . Y.encode
