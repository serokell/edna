module Edna.DB.Schema
  ( ensureSchemaIsSetUp
  ) where

import Universum

import Data.FileEmbed (embedStringFile)
import Database.Beam.Backend (MonadBeam, runNoReturn)
import Database.Beam.Postgres (Postgres)
import Database.Beam.Postgres.Syntax (PgCommandSyntax(..), PgCommandType(..), emit)

ensureSchemaIsSetUp :: MonadBeam Postgres m => m ()
ensureSchemaIsSetUp = runNoReturn $
  PgCommandSyntax PgCommandTypeDataUpdate $ emit $(embedStringFile "sql/initial_schema.sql")
