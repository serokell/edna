{-# LANGUAGE DataKinds        #-}

{-|
Definition of Edna configuration.
-}
module Edna.Config
       ( EdnaConfig (..)
       , ApiConfig (..)
       ) where

import Universum

import Data.Aeson.TH (deriveJSON)

import Edna.Util (NetworkAddress, ednaAesonOptions)

data ApiConfig = ApiConfig
  { acListenAddr :: NetworkAddress
  , acServeDocs :: Bool
  } deriving stock (Generic, Read, Show)

data EdnaConfig = EdnaConfig {
  ecApi :: ApiConfig
} deriving stock (Generic, Read, Show)

---------------------------------------------------------------------------
-- Derivations
---------------------------------------------------------------------------

deriveJSON ednaAesonOptions ''ApiConfig
deriveJSON ednaAesonOptions ''EdnaConfig
