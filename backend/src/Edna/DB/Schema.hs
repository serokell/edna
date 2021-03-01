module Edna.DB.Schema
  ( ensureSchemaIsSetUp
  , resetSchema
  ) where

import Universum

import Data.FileEmbed (embedStringFile)
import Database.Beam.Backend (MonadBeam, runNoReturn)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgCommandSyntax(..), PgCommandType(..), emit)

ensureSchemaIsSetUp :: MonadBeam Postgres m => m ()
ensureSchemaIsSetUp = runNoReturn $
  PgCommandSyntax PgCommandTypeDataUpdate $ emit $(embedStringFile "sql/initial_schema.sql")

resetSchema :: MonadBeam Postgres m => m ()
resetSchema = runNoReturn $
  PgCommandSyntax PgCommandTypeDataUpdate $
  emit "set client_min_messages to warning;\
       \drop schema public cascade;\
       \set client_min_messages to notice;\
       \create schema public;"
