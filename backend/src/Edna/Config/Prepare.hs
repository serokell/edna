-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Edna.Config.Prepare
  ( prepareConfig
  ) where

import Universum

import qualified Data.Yaml as Y

import Edna.Config.CLA (EdnaOptions(..))
import Edna.Config.Definition
  (ApiConfig(..), DbConfig(..), DbInit(..), EdnaConfig(..), LoggingConfig, defaultEdnaConfig)
import Edna.Config.Environment
  (apiListenAddrEnv, apiServeDocsEnv, dbConnStringEnv, dbInitialisationInitScriptEnv,
  dbInitialisationModeEnv, dbMaxConnectionsEnv, loggingEnv)

choose :: a -> Maybe a -> IO (Maybe a) -> IO a
choose _ (Just claOption) _ = return claOption
choose configOption Nothing envOption = do
  env <- envOption
  case env of
    Nothing -> return configOption
    Just x -> return x

chooseMaybe :: Maybe a -> IO (Maybe a) -> IO (Maybe a)
chooseMaybe (Just claOption) _ = return $ Just claOption
chooseMaybe Nothing envOption = do
  env <- envOption
  case env of
    Nothing -> return Nothing
    Just x -> return $ Just x

mconcatMaybe :: Monoid a => a -> [Maybe a] -> IO a
mconcatMaybe value list = return $ maybe value (value <>) $ mconcat list


-- | Prepare 'EdnaConfig', priority - CLA, env, config file
prepareConfig :: EdnaOptions -> IO EdnaConfig
prepareConfig options = do
  -- use config file if specified or default config otherwise
  config <- maybe (pure defaultEdnaConfig) Y.decodeFileThrow $
              eoConfig options

  api <- prepareApiConfig (_ecApi config) options
  db <- prepareDbConfig (_ecDb config) options
  logging <- prepareLoggingConfig (_ecLogging config) options

  return $ EdnaConfig api db logging


prepareApiConfig :: ApiConfig -> EdnaOptions -> IO ApiConfig
prepareApiConfig config options = do
  listen <- choose
            (_acListenAddr config)
            (eoApiListenAddr options)
            apiListenAddrEnv
  docs <- choose
            (_acServeDocs config)
            (eoApiServeDocs options)
            apiServeDocsEnv
  return $ ApiConfig listen docs


prepareDbConfig :: DbConfig -> EdnaOptions -> IO DbConfig
prepareDbConfig config options = do
  envConn <- dbConnStringEnv
  connStr <- mconcatMaybe
              (_dbConnString config)
              [envConn, eoDbConnString options]
  maxConn <- choose
              (_dbMaxConnections config)
              (eoDbMaxConnections options)
              dbMaxConnectionsEnv
  dbInit <- prepareDbInitConfig (_dbInitialisation config) options
  return $ DbConfig connStr maxConn dbInit

prepareDbInitConfig :: Maybe DbInit -> EdnaOptions -> IO (Maybe DbInit)
prepareDbInitConfig (Just config) options = do
  mode <- choose
            (_dbiMode config)
            (eoDbInitialisationMode options)
            dbInitialisationModeEnv
  script <- choose
              (_dbiInitScript config)
              (eoDbInitialisationInitScript options)
              dbInitialisationInitScriptEnv
  return $ Just (DbInit mode script)
prepareDbInitConfig Nothing options = do
  mode <- chooseMaybe
            (eoDbInitialisationMode options)
            dbInitialisationModeEnv
  script <- chooseMaybe
              (eoDbInitialisationInitScript options)
              dbInitialisationInitScriptEnv
  return $ DbInit <$> mode <*> script


prepareLoggingConfig :: LoggingConfig -> EdnaOptions -> IO LoggingConfig
prepareLoggingConfig config options = do
  choose config (eoLogging options) loggingEnv
