{-# LANGUAGE DataKinds #-}

{-|
Definition of Edna configuration.
-}
module Edna.Config.Definition
  ( defaultEdnaConfig

  , EdnaConfig (..)
  , ecApi
  , ecDb
  , ecLogging

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

  , LoggingConfig (..)
  ) where

import Universum

import qualified Data.Aeson as Aeson
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

-- | Specification of how much to log.
data LoggingConfig =
    LogDev
  -- ^ Development logging mode: log a lot.
  | LogProd
  -- ^ Production logging mode: log less than in development mode.
  | LogNothing
  -- ^ No logging.
  deriving stock (Generic, Show)

loggingConfigAesonOptions :: Aeson.Options
loggingConfigAesonOptions = Aeson.defaultOptions
  { Aeson.sumEncoding = Aeson.UntaggedValue
  , Aeson.constructorTagModifier = drop 3
  }

instance Aeson.FromJSON LoggingConfig where
  parseJSON = Aeson.genericParseJSON loggingConfigAesonOptions

instance Aeson.ToJSON LoggingConfig where
  toEncoding = Aeson.genericToEncoding loggingConfigAesonOptions

data EdnaConfig = EdnaConfig
  { _ecApi :: ApiConfig
  , _ecDb :: DbConfig
  , _ecLogging :: LoggingConfig
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
  , _ecLogging = LogProd
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
