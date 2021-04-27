-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Edna.Config.CLA
  ( EdnaOptions(..)
  , ednaOpts
  ) where

import Universum

import Options.Applicative
  (Parser, ParserInfo, auto, eitherReader, flag', fullDesc, help, helper, info, long, metavar,
  option, progDesc, short, str, strOption, switch)

import Edna.Config.Definition (LoggingConfig(..), parseLoggingConfig)
import Edna.Util (ConnString(..), DatabaseInitOption, NetworkAddress, parseDatabaseInitOption)

ednaOpts :: ParserInfo EdnaOptions
ednaOpts = info (ednaOpts' <**> helper) $
  fullDesc <> progDesc "Edna API server"

data EdnaOptions = EdnaOptions
  { eoConfig :: Maybe FilePath
  , eoApiListenAddr :: Maybe NetworkAddress
  , eoApiServeDocs :: Maybe Bool
  , eoDbConnString :: Maybe ConnString
  , eoDbMaxConnections :: Maybe Int
  , eoDbInitialisationMode :: Maybe DatabaseInitOption
  , eoDbInitialisationInitScript :: Maybe FilePath
  , eoLogging :: Maybe LoggingConfig
  , eoDumpConfig :: Bool
  } deriving stock (Generic, Show)


ednaOpts' :: Parser EdnaOptions
ednaOpts' = EdnaOptions
        <$> configParser
        <*> apiListenAddrParser
        <*> apiServeDocsParser
        <*> dbConnStringParser
        <*> dbMaxConnectionsParser
        <*> dbInitialisationModeParser
        <*> dbInitialisationInitScriptParser
        <*> loggingParser
        <*> dumpConfigParser


configParser :: Parser (Maybe FilePath)
configParser = optional $ strOption $
  short 'c' <>
  long "config" <>
  metavar "EDNA_CONFIG" <>
  help "Path to configuration file."

apiListenAddrParser :: Parser (Maybe NetworkAddress)
apiListenAddrParser = optional $ option auto $
  long "listen-addr" <>
  metavar "EDNA_API_LISTEN_ADDR" <>
  help "Edna API listen address (host:port)."

apiServeDocsParser :: Parser (Maybe Bool)
apiServeDocsParser = optional $ flag' True $
  long "serve-docs" <>
  help "Whether to serve docs."

dbConnStringParser :: Parser (Maybe ConnString)
dbConnStringParser = optional $ option (ConnString <$> str) $
  long "conn-string" <>
  metavar "EDNA_DB_CONN_STRING" <>
  help "Edna DB connection string."

dbMaxConnectionsParser :: Parser (Maybe Int)
dbMaxConnectionsParser = optional $ option auto $
  long "max-connections" <>
  metavar "EDNA_DB_MAX_CONNECTIONS" <>
  help "Edna DB max connections."

dbInitialisationModeParser :: Parser (Maybe DatabaseInitOption)
dbInitialisationModeParser = optional $ option (eitherReader parser) $
  long "init-mode" <>
  metavar "EDNA_DB_INITIALISATION_MODE" <>
  help "Edna DB initialisation mode: \"enable\" or \"enable-with-drop\"."
  where
    parser :: String -> Either String DatabaseInitOption
    parser = parseDatabaseInitOption "Can't parse EDNA_DB_INITIALISATION_MODE"

dbInitialisationInitScriptParser :: Parser (Maybe FilePath)
dbInitialisationInitScriptParser = optional $ strOption $
  long "init-script" <>
  metavar "EDNA_DB_INITIALISATION_INIT_SCRIPT" <>
  help "Path to DB init script."

loggingParser :: Parser (Maybe LoggingConfig)
loggingParser = optional $ option (eitherReader parser) $
  short 'l' <>
  long "logging" <>
  metavar "EDNA_LOGGING" <>
  help "Edna logging level: dev, prod or nothing."
  where
    parser :: String -> Either String LoggingConfig
    parser = parseLoggingConfig "Can't parse EDNA_LOGGING"

dumpConfigParser :: Parser Bool
dumpConfigParser = switch $
  long "dump-config" <>
  help "Dump config and exit."
