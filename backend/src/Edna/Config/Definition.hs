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

  , DbInit (..)
  , dbiMode
  , dbiInitScript

  , DbConfig (..)
  , dbConnString
  , dbMaxConnections
  , dbInitialisation
  ) where

import Universum

import Data.Aeson.TH (deriveJSON)
import Lens.Micro.Platform (makeLenses)
import Text.Read (read)

import Edna.Util (ConnString(..), DatabaseInitOption(..), NetworkAddress, ednaAesonConfigOptions)

data ApiConfig = ApiConfig
  { _acListenAddr :: NetworkAddress
  , _acServeDocs :: Bool
  } deriving stock (Generic, Show)

data DbInit = DbInit
  { _dbiMode :: DatabaseInitOption
  , _dbiInitScript :: FilePath
  } deriving stock (Generic, Show)

data DbConfig = DbConfig
  { _dbConnString :: ConnString
  , _dbMaxConnections :: Int
  , _dbInitialisation :: Maybe DbInit
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
    , _dbMaxConnections = 200
    , _dbInitialisation = Nothing
    }
  }

---------------------------------------------------------------------------
-- Derivations
---------------------------------------------------------------------------

deriveJSON ednaAesonConfigOptions ''ApiConfig
deriveJSON ednaAesonConfigOptions ''DbInit
deriveJSON ednaAesonConfigOptions ''DbConfig
deriveJSON ednaAesonConfigOptions ''EdnaConfig

makeLenses ''ApiConfig
makeLenses ''DbInit
makeLenses ''DbConfig
makeLenses ''EdnaConfig
