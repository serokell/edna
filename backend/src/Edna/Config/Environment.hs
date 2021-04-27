-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Edna.Config.Environment
  ( apiListenAddrEnv
  , apiServeDocsEnv
  , askDebugDB
  , dbConnStringEnv
  , dbInitialisationInitScriptEnv
  , dbInitialisationModeEnv
  , dbMaxConnectionsEnv
  , loggingEnv
  ) where

import Universum

import Control.Exception (throwIO)
import qualified Data.Char as C
import System.Environment (lookupEnv)
import qualified Text.Show

import Edna.Config.Definition (LoggingConfig(..), parseLoggingConfig)
import Edna.Util (ConnString(..), DatabaseInitOption, NetworkAddress, parseDatabaseInitOption)

data EnvParseError = WrongApiListenAddr
                   | WrongDbMaxConnections
                   | WrongDbInitialisationMode
                   | WrongLogging

instance Show EnvParseError where
  show WrongApiListenAddr = "Can't parse EDNA_API_LISTEN_ADDR"
  show WrongDbMaxConnections = "Can't parse EDNA_DB_MAX_CONNECTIONS"
  show WrongDbInitialisationMode = "Can't parse EDNA_DB_INITIALISATION_MODE"
  show WrongLogging   = "Can't parse EDNA_LOGGING"

instance Exception EnvParseError

parseEnv :: (EnvParseError -> String -> Either EnvParseError a)
         -> String
         -> EnvParseError
         -> IO (Maybe a)
parseEnv parser name err = do
  env <- lookupEnv name
  case parser err <$> env of
    Nothing -> return Nothing
    Just (Right value) -> return $ Just value
    Just (Left e) -> throwIO e

readEnv :: Read a => String -> EnvParseError -> IO (Maybe a)
readEnv = parseEnv read
  where
    read err name = case readEither name of
      Left _ -> Left err
      Right x -> Right x

getBoolEnv :: String -> IO (Maybe Bool)
getBoolEnv name = lookupEnv name <&> \case
  Nothing                        -> Nothing
  Just "1"                       -> Just True
  Just (map C.toLower -> "true") -> Just True
  _                              -> Just False

-- If @EDNA_DB_DEBUG@ environment variable is set to @1@
-- or @TRUE@ (case-insensitive), we will run postgres with
-- debug logging.
{-# ANN askDebugDB ("HLint: ignore Use Just" :: Text) #-}
askDebugDB :: IO Bool
askDebugDB = do
  env <- getBoolEnv "EDNA_DB_DEBUG"
  return $ fromMaybe False env


apiListenAddrEnv :: IO (Maybe NetworkAddress)
apiListenAddrEnv = readEnv "EDNA_API_LISTEN_ADDR" WrongApiListenAddr

apiServeDocsEnv :: IO (Maybe Bool)
apiServeDocsEnv = getBoolEnv "EDNA_API_SERVE_DOCS"

dbConnStringEnv :: IO (Maybe ConnString)
dbConnStringEnv = do
  host <- lookupEnv "POSTGRES_HOST"
  port <- lookupEnv "POSTGRES_PORT"
  user <- lookupEnv "POSTGRES_USER"
  password <- lookupEnv "POSTGRES_PASSWORD"
  db <- lookupEnv "POSTGRES_DB"

  if all isNothing [host, port, user, password, db]
  then return Nothing
  else return $ ConnString <$>
    mconcat [ buildConnection "host=" host
            , buildConnection " port=" port
            , buildConnection " dbname=" db
            , buildConnection " user=" user
            , buildConnection " password=" password
            ]
  where
    buildConnection :: ByteString -> Maybe String -> Maybe ByteString
    buildConnection option env = (option <>) . encodeUtf8 <$> env

dbMaxConnectionsEnv :: IO (Maybe Int)
dbMaxConnectionsEnv = readEnv "EDNA_DB_MAX_CONNECTIONS" WrongDbMaxConnections

dbInitialisationModeEnv :: IO (Maybe DatabaseInitOption)
dbInitialisationModeEnv = parseEnv parseDatabaseInitOption
  "EDNA_DB_INITIALISATION_MODE" WrongDbInitialisationMode

dbInitialisationInitScriptEnv :: IO (Maybe FilePath)
dbInitialisationInitScriptEnv = lookupEnv "EDNA_DB_INITIALISATION_INIT_SCRIPT"

loggingEnv :: IO (Maybe LoggingConfig)
loggingEnv = parseEnv parseLoggingConfig "EDNA_LOGGING" WrongLogging
