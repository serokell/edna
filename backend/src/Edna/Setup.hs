module Edna.Setup
  ( EdnaContext (..)
  , Edna
  , edConfig
  , runEdna
  ) where

import Universum

import Lens.Micro.Platform (makeLenses)

import Edna.Config.Definition (EdnaConfig(..))
import RIO (RIO, runRIO)

data EdnaContext = EdnaContext
  { _edConfig :: !EdnaConfig
  }

makeLenses ''EdnaContext

type Edna = RIO EdnaContext

runEdna :: EdnaConfig -> Edna a -> IO a
runEdna config action = do
  let ednaContext = EdnaContext
        { _edConfig = config
        }
  runRIO ednaContext action
