-- | Haskell representation of Edna DB.

module Edna.DB.Schema
  ( EdnaSchema (..)
  , ednaSchema
  ) where

import Universum

import Database.Beam.Postgres (Postgres)
import Database.Beam.Schema (Database, DatabaseSettings, TableEntity, defaultDbSettings)

import Edna.Dashboard.DB.Schema
  (AnalysisMethodT, ExperimentT, MeasurementT, PrimarySubExperimentT, RemovedMeasurementsT,
  SubExperimentT)
import Edna.Library.DB.Schema (CompoundT, ProjectT, TargetT, TestMethodologyT)
import Edna.Upload.DB.Schema (ExperimentFileT)

--------------------------
-- Enda Schema
--------------------------

data EdnaSchema f = EdnaSchema
  { esProject :: f (TableEntity ProjectT)
  , esTestMethodology :: f (TableEntity TestMethodologyT)
  , esTarget :: f (TableEntity TargetT)
  , esCompound :: f (TableEntity CompoundT)
  , esExperimentFile :: f (TableEntity ExperimentFileT)
  , esExperiment :: f (TableEntity ExperimentT)
  , esMeasurement :: f (TableEntity MeasurementT)
  , esAnalysisMethod :: f (TableEntity AnalysisMethodT)
  , esSubExperiment :: f (TableEntity SubExperimentT)
  , esPrimarySubExperiment :: f (TableEntity PrimarySubExperimentT)
  , esRemovedMeasurements :: f (TableEntity RemovedMeasurementsT)
  } deriving stock (Generic)
    deriving anyclass (Database be)

ednaSchema :: DatabaseSettings Postgres EdnaSchema
ednaSchema = defaultDbSettings
