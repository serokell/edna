-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Edna.DB.Initialisation
  ( schemaInit
  , schemaSetup
  , resetSchema
  ) where

import Universum

import qualified Data.ByteString as BS

import Database.Beam.Backend (MonadBeam, runNoReturn)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgCommandSyntax(..), PgCommandType(..), emit)

import Edna.Config.Definition (DbInit(..), dbInitialisation, ecDb)
import Edna.DB.Integration (runPg, transact)
import Edna.Setup (Edna, fromConfig)
import Edna.Util (DatabaseInitOption(..))

schemaInit :: Edna ()
schemaInit = do
  dbInit <- fromConfig $ ecDb . dbInitialisation
  let runInit filePath f = liftIO (BS.readFile filePath) >>= \script -> transact $ runPg $ f script
  case dbInit of
    Nothing -> pure ()
    Just (DbInit Enable fp) -> runInit fp schemaSetup
    Just (DbInit EnableWithDrop fp) -> runInit fp $ \s -> resetSchema >> schemaSetup s

schemaSetup :: MonadBeam Postgres m => ByteString -> m ()
schemaSetup = runNoReturn . PgCommandSyntax PgCommandTypeDataUpdate . emit

resetSchema :: MonadBeam Postgres m => m ()
resetSchema = runNoReturn $
  PgCommandSyntax PgCommandTypeDataUpdate $
  emit "set client_min_messages to warning;\
       \drop schema public cascade;\
       \set client_min_messages to notice;\
       \create schema public;"
