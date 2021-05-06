-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

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
  , ecMdeHost

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
  , parseLoggingConfig

  , MdeHost
  ) where

import Universum

import qualified Data.Aeson as Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.Char as C
import qualified Data.Text as T
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
  deriving stock (Generic, Show, Eq)

type MdeHost = Text

-- | Parse LoggingConfig
parseLoggingConfig :: e -> String -> Either e LoggingConfig
parseLoggingConfig err p =
  case value of
    "dev"     -> Right LogDev
    "prod"    -> Right LogProd
    "nothing" -> Right LogNothing
    _         -> Left err
  where
    value = T.toLower (T.pack p)

loggingConfigAesonOptions :: Aeson.Options
loggingConfigAesonOptions = Aeson.defaultOptions
  { Aeson.sumEncoding = Aeson.UntaggedValue
  , Aeson.constructorTagModifier = map C.toLower . drop 3
  }

instance Aeson.FromJSON LoggingConfig where
  parseJSON = Aeson.withText "LoggingConfig" $ \text ->
    let value = parseLoggingConfig Nothing $ T.unpack text
    in case value of
      Right x -> return x
      Left _  -> fail $ "Invalid config value: " <> show text

instance Aeson.ToJSON LoggingConfig where
  toEncoding = Aeson.genericToEncoding loggingConfigAesonOptions
  toJSON = Aeson.genericToJSON loggingConfigAesonOptions

data EdnaConfig = EdnaConfig
  { _ecApi :: ApiConfig
  , _ecDb :: DbConfig
  , _ecLogging :: LoggingConfig
  , _ecMdeHost :: Maybe MdeHost
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
  , _ecMdeHost = Nothing
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
