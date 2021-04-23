-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Edna.Setup
  ( Edna
  , runEdna

  , EdnaContext (..)
  , edConfig
  , edConnectionPool
  , edDebugDB
  ) where

import Universum

import qualified Data.Char as C

import Lens.Micro.Platform (makeLenses)
import RIO (RIO, runRIO)
import System.Environment (lookupEnv)

import Edna.Config.Definition (EdnaConfig(..))
import Edna.DB.Connection (ConnPool(..), withPostgresConn)

data EdnaContext = EdnaContext
  { _edConfig :: !EdnaConfig
  , _edConnectionPool :: !ConnPool
  , _edDebugDB :: !Bool
  }

makeLenses ''EdnaContext

type Edna = RIO EdnaContext

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

-- If @EDNA_DEBUG_DB@ environment variable is set to @1@
-- or @TRUE@ (case-insensitive), we will run postgres with
-- debug logging.
askDebugDB :: IO Bool
askDebugDB = lookupEnv "EDNA_DEBUG_DB" <&> \case
  Just "1"                       -> True
  Just (map C.toLower -> "true") -> True
  _                              -> False
