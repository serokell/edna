{-# LANGUAGE DataKinds #-}

{-|
Definition of Edna configuration.
-}
module Edna.Config.Definition
  ( defaultEdnaConfig
  
  , EdnaConfig (..)
  , ecApi
  , ecDb
  
  , ApiConfig (..)
  , acListenAddr
  , acServeDocs
  
  , DbConfig (..)
  , dbConnString
  , dbMaxConnection
  ) where

import Universum

import Data.Aeson.TH (deriveJSON)
import Text.Read (read)
import Lens.Micro.Platform (makeLenses)

import Edna.Util (NetworkAddress, ednaAesonConfigOptions, ConnString (..))

data ApiConfig = ApiConfig
  { _acListenAddr :: NetworkAddress
  , _acServeDocs :: Bool
  } deriving stock (Generic, Show)

data DbConfig = DbConfig
  { _dbConnString :: ConnString
  , _dbMaxConnection :: Int
  } deriving stock (Generic, Show)

data EdnaConfig = EdnaConfig
  { _ecApi :: ApiConfig
  , _ecDb :: DbConfig
} deriving stock (Generic, Show)

defaultEdnaConfig :: EdnaConfig
defaultEdnaConfig = EdnaConfig
  { _ecApi = ApiConfig
    { _acListenAddr = read "*:9000"
    , _acServeDocs = True
    }
  , _ecDb = DbConfig
    { _dbConnString = ConnString "host=/run/postgresql dbname=edna"
    , _dbMaxConnection = 200
    }
  }

---------------------------------------------------------------------------
-- Derivations
---------------------------------------------------------------------------

deriveJSON ednaAesonConfigOptions ''ApiConfig
deriveJSON ednaAesonConfigOptions ''DbConfig
deriveJSON ednaAesonConfigOptions ''EdnaConfig

makeLenses ''ApiConfig
makeLenses ''DbConfig
makeLenses ''EdnaConfig
