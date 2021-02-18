{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}

{-|
Definition of Edna configuration.
-}
module Edna.Config
       ( EdnaConfig (..)
       , ApiConfig (..)
       ) where

import Universum

import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Options (defaultOptions)

import Edna.Util (NetworkAddress)

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

deriveJSON defaultOptions ''ApiConfig
deriveJSON defaultOptions ''EdnaConfig
