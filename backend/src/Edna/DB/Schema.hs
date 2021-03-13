module Edna.DB.Schema
  ( schemaSetup
  , resetSchema
  , schemaInit
  ) where

import Universum

import qualified Data.ByteString as BS

import Database.Beam.Backend (MonadBeam, runNoReturn)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgCommandSyntax(..), PgCommandType(..), emit)

import Edna.Config.Definition (DbInitiation(..), dbInitiation, ecDb)
import Edna.Config.Utils (fromConfig)
import Edna.DB.Integration (runPg, transact)
import Edna.Setup (Edna)
import Edna.Util (DatabaseInitOption(..))

schemaInit :: Edna ()
schemaInit = do
  dbInit <- fromConfig $ ecDb . dbInitiation
  let runInit filePath f = liftIO (BS.readFile filePath) >>= \script -> transact $ runPg $ f script
  case dbInit of
    Nothing -> pure ()
    Just (DbInitiation Enable fp) -> runInit fp schemaSetup
    Just (DbInitiation EnableWithDrop fp) -> runInit fp $ \s -> resetSchema >> schemaSetup s

schemaSetup :: MonadBeam Postgres m => ByteString -> m ()
schemaSetup = runNoReturn . PgCommandSyntax PgCommandTypeDataUpdate . emit

resetSchema :: MonadBeam Postgres m => m ()
resetSchema = runNoReturn $
  PgCommandSyntax PgCommandTypeDataUpdate $
  emit "set client_min_messages to warning;\
       \drop schema public cascade;\
       \set client_min_messages to notice;\
       \create schema public;"
