-- | Haskell representation of Edna DB.

module Edna.DB.Schema
  ( ProjectT (..)
  , ProjectRec

  , TestMethodologyT (..)
  , TestMethodologyRec

  , TargetT (..)
  , TargetRec

  , CompoundT (..)
  , CompoundRec

  , ExperimentFileT (..)
  , ExperimentFileRec

  , ExperimentT (..)
  , ExperimentRec

  , MeasurementT (..)
  , MeasurementRec

  , AnalysisMethodT (..)
  , AnalysisMethodRec
  , theOnlyAnalysisMethod
  , theOnlyAnalysisMethodId

  , SubExperimentT (..)
  , SubExperimentRec

  , RemovedMeasurementsT (..)
  , RemovedMeasurementsRec

  , PrimaryKey (..)

  , EdnaSchema (..)
  , ednaSchema
  ) where

import Universum

import Data.Time (LocalTime)
import Database.Beam.Backend.SQL.BeamExtensions (SqlSerial(..))
import Database.Beam.Backend.Types (Nullable)
import Database.Beam.Postgres (PgJSON(..), Postgres)
import Database.Beam.Schema
  (Beamable, C, Database, DatabaseSettings, Table(..), TableEntity, defaultDbSettings)

import Edna.ExperimentReader.Types (FileMetadata)

--------------------------
-- Project
--------------------------

data ProjectT f = ProjectRec
  { pProjectId :: C f (SqlSerial Word32)
  , pName :: C f Text
  , pDescription :: C (Nullable f) Text
  , pCreationDate :: C f LocalTime
  , pLastUpdate :: C f LocalTime
  } deriving stock Generic
    deriving anyclass Beamable

type ProjectRec = ProjectT Identity

deriving stock instance Show ProjectRec
deriving stock instance Eq ProjectRec

instance Table ProjectT where
  data PrimaryKey ProjectT f = ProjectId (C f (SqlSerial Word32))
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = ProjectId . pProjectId

deriving stock instance Show (PrimaryKey ProjectT Identity)
deriving stock instance Eq (PrimaryKey ProjectT Identity)

--------------------------
-- Test methodology
--------------------------

data TestMethodologyT f = TestMethodologyRec
  { tmTestMethodologyId :: C f (SqlSerial Word32)
  , tmName :: C f Text
  , tmDescription :: C (Nullable f) Text
  , tmConfluenceLink :: C (Nullable f) Text
  } deriving stock Generic
    deriving anyclass Beamable

type TestMethodologyRec = TestMethodologyT Identity

deriving stock instance Show TestMethodologyRec
deriving stock instance Eq TestMethodologyRec

instance Table TestMethodologyT where
  data PrimaryKey TestMethodologyT f = TestMethodologyId (C f (SqlSerial Word32))
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = TestMethodologyId . tmTestMethodologyId

deriving stock instance Show (PrimaryKey TestMethodologyT Identity)
deriving stock instance Eq (PrimaryKey TestMethodologyT Identity)

--------------------------
-- Target
--------------------------

data TargetT f = TargetRec
  { tTargetId :: C f (SqlSerial Word32)
  , tName :: C f Text
  , tCreationDate :: C f LocalTime
  } deriving stock Generic
    deriving anyclass Beamable

type TargetRec = TargetT Identity

deriving stock instance Show TargetRec
deriving stock instance Eq TargetRec

instance Table TargetT where
  data PrimaryKey TargetT f = TargetId (C f (SqlSerial Word32))
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = TargetId . tTargetId

deriving stock instance Show (PrimaryKey TargetT Identity)
deriving stock instance Eq (PrimaryKey TargetT Identity)

--------------------------
-- Compound
--------------------------

data CompoundT f = CompoundRec
  { cCompoundId :: C f (SqlSerial Word32)
  , cName :: C f Text
  , cCreationDate :: C f LocalTime
  , cChemsoftLink :: C (Nullable f) Text
  } deriving stock Generic
    deriving anyclass Beamable

type CompoundRec = CompoundT Identity

deriving stock instance Show CompoundRec
deriving stock instance Eq CompoundRec

instance Table CompoundT where
  data PrimaryKey CompoundT f = CompoundId (C f (SqlSerial Word32))
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = CompoundId . cCompoundId

deriving stock instance Show (PrimaryKey CompoundT Identity)
deriving stock instance Eq (PrimaryKey CompoundT Identity)

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

--------------------------
-- Experiment
--------------------------

data ExperimentT f = ExperimentRec
  { eExperimentId :: C f (SqlSerial Word32)
  , eExperimentFileId :: C f Word32
  , eCompoundId :: C f Word32
  , eTargetId :: C f Word32
  } deriving stock Generic
    deriving anyclass Beamable

type ExperimentRec = ExperimentT Identity

deriving stock instance Show ExperimentRec
deriving stock instance Eq ExperimentRec

instance Table ExperimentT where
  data PrimaryKey ExperimentT f = ExperimentId (C f (SqlSerial Word32))
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = ExperimentId . eExperimentId

deriving stock instance Show (PrimaryKey ExperimentT Identity)
deriving stock instance Eq (PrimaryKey ExperimentT Identity)

--------------------------
-- Measurement
--------------------------

data MeasurementT f = MeasurementRec
  { mMeasurementId :: C f (SqlSerial Word32)
  , mExperimentId :: C f Word32
  , mConcentration :: C f Double
  , mSignal :: C f Double
  , mIsOutlier :: C f Bool
  } deriving stock Generic
    deriving anyclass Beamable

type MeasurementRec = MeasurementT Identity

deriving stock instance Show MeasurementRec
deriving stock instance Eq MeasurementRec

instance Table MeasurementT where
  data PrimaryKey MeasurementT f = MeasurementId (C f (SqlSerial Word32))
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = MeasurementId . mMeasurementId

deriving stock instance Show (PrimaryKey MeasurementT Identity)
deriving stock instance Eq (PrimaryKey MeasurementT Identity)

--------------------------
-- Analysis Method
--------------------------

data AnalysisMethodT f = AnalysisMethodRec
  { amAnalysisMethodId :: C f (SqlSerial Word32)
  , amDescription :: C (Nullable f) Text
  , amParameters :: C f (PgJSON ()) -- TODO add corresponding type
  } deriving stock Generic
    deriving anyclass Beamable

type AnalysisMethodRec = AnalysisMethodT Identity

deriving stock instance Show AnalysisMethodRec
deriving stock instance Eq AnalysisMethodRec

instance Table AnalysisMethodT where
  data PrimaryKey AnalysisMethodT f = AnalysisMethodId (C f (SqlSerial Word32))
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = AnalysisMethodId . amAnalysisMethodId

deriving stock instance Show (PrimaryKey AnalysisMethodT Identity)
deriving stock instance Eq (PrimaryKey AnalysisMethodT Identity)

-- | Currently we always have only one analysis method that is inserted upon
-- initialization.
theOnlyAnalysisMethod :: AnalysisMethodRec
theOnlyAnalysisMethod = AnalysisMethodRec
  { amAnalysisMethodId = 1
  , amDescription = Just "IC50"
  , amParameters = PgJSON ()
  }

-- | Currently we always have only one analysis method that is inserted upon
-- initialization. This value is its ID.
theOnlyAnalysisMethodId :: Word32
theOnlyAnalysisMethodId = unSerial (amAnalysisMethodId theOnlyAnalysisMethod)

--------------------------
-- Sub-experiment
--------------------------

data SubExperimentT f = SubExperimentRec
  { seSubExperimentId :: C f (SqlSerial Word32)
  , seAnalysisMethodId :: C f Word32
  , seExperimentId :: C f Word32
  , seIsSuspicious :: C f Bool
  , seResult :: C f (PgJSON Double)
  } deriving stock Generic
    deriving anyclass Beamable

type SubExperimentRec = SubExperimentT Identity

deriving stock instance Show SubExperimentRec
deriving stock instance Eq SubExperimentRec

instance Table SubExperimentT where
  data PrimaryKey SubExperimentT f = SubExperimentId (C f (SqlSerial Word32))
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = SubExperimentId . seSubExperimentId

deriving stock instance Show (PrimaryKey SubExperimentT Identity)
deriving stock instance Eq (PrimaryKey SubExperimentT Identity)

-------------------------
-- Removed Measurements
-------------------------

data RemovedMeasurementsT f = RemovedMeasurementsRec
  { rmSubExperimentId :: C f Word32
  , rmMeasurementId :: C f Word32
  } deriving stock Generic
    deriving anyclass Beamable

type RemovedMeasurementsRec = RemovedMeasurementsT Identity

deriving stock instance Show RemovedMeasurementsRec
deriving stock instance Eq RemovedMeasurementsRec

instance Table RemovedMeasurementsT where
  data PrimaryKey RemovedMeasurementsT f = RemovedMeasurementsId (C f Word32) (C f Word32)
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = RemovedMeasurementsId <$> rmSubExperimentId <*> rmMeasurementId

deriving stock instance Show (PrimaryKey RemovedMeasurementsT Identity)
deriving stock instance Eq (PrimaryKey RemovedMeasurementsT Identity)

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
  , esRemovedMeasurements :: f (TableEntity RemovedMeasurementsT)
  } deriving stock (Generic)
    deriving anyclass (Database be)

ednaSchema :: DatabaseSettings Postgres EdnaSchema
ednaSchema = defaultDbSettings
