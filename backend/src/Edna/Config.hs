{-# LANGUAGE DataKinds        #-}

{-|
Definition of Edna configuration.
-}
module Edna.Config
  ( EdnaConfig (..)
  , ApiConfig (..)
  , defaultEdnaConfig
  ) where

import Universum

import Text.Read (read)
import Data.Aeson.TH (deriveJSON)

import Edna.Util (NetworkAddress, ednaAesonConfigOptions)

data ApiConfig = ApiConfig
  { acListenAddr :: NetworkAddress
  , acServeDocs :: Bool
  } deriving stock (Generic, Show)

newtype EdnaConfig = EdnaConfig {
  ecApi :: ApiConfig
} deriving stock (Generic, Show)

defaultEdnaConfig :: EdnaConfig
defaultEdnaConfig = EdnaConfig
  { ecApi = ApiConfig
    { acListenAddr = read "*:9000"
    , acServeDocs = True
    }
  }

---------------------------------------------------------------------------
-- Derivations
---------------------------------------------------------------------------

deriveJSON ednaAesonConfigOptions ''ApiConfig
deriveJSON ednaAesonConfigOptions ''EdnaConfig
