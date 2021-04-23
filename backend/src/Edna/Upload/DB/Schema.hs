-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Edna.Upload.DB.Schema
  ( ExperimentFileT (..)
  , ExperimentFileRec

  , PrimaryKey (..)
  ) where

import Universum

import Data.Time (LocalTime)
import Database.Beam.Backend.SQL.BeamExtensions (SqlSerial(..))
import Database.Beam.Postgres (PgJSON(..))
import Database.Beam.Schema (Beamable, C, Nullable, Table(..))

import Edna.ExperimentReader.Types (FileMetadata)

--------------------------
-- Experiment File
--------------------------

data ExperimentFileT f = ExperimentFileRec
  { efExperimentFileId :: C f (SqlSerial Word32)
  , efProjectId :: C f Word32
  , efMethodologyId :: C (Nullable f) Word32
  , efUploadDate :: C f LocalTime
  , efMeta :: C f (PgJSON FileMetadata)
  , efDescription :: C f Text
  , efName :: C f Text
  , efContents :: C f LByteString
  } deriving stock Generic
    deriving anyclass Beamable

type ExperimentFileRec = ExperimentFileT Identity

deriving stock instance Show ExperimentFileRec
deriving stock instance Eq ExperimentFileRec

instance Table ExperimentFileT where
  data PrimaryKey ExperimentFileT f = ExperimentFileId (C f (SqlSerial Word32))
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = ExperimentFileId . efExperimentFileId

deriving stock instance Show (PrimaryKey ExperimentFileT Identity)
deriving stock instance Eq (PrimaryKey ExperimentFileT Identity)
