-- | Dashboard-specific part of DB schema.

module Edna.Dashboard.DB.Schema
  ( ExperimentT (..)
  , ExperimentRec

  , MeasurementT (..)
  , MeasurementRec

  , AnalysisMethodT (..)
  , AnalysisMethodRec
  , theOnlyAnalysisMethod
  , theOnlyAnalysisMethodId

  , SubExperimentT (..)
  , SubExperimentRec

  , PrimarySubExperimentT (..)
  , PrimarySubExperimentRec

  , RemovedMeasurementsT (..)
  , RemovedMeasurementsRec

  , PrimaryKey (..)
  ) where

import Universum

import Database.Beam.Backend.SQL.BeamExtensions (SqlSerial(..))
import Database.Beam.Backend.Types (Nullable)
import Database.Beam.Postgres (PgJSON(..))
import Database.Beam.Schema (Beamable, C, Table(..))

import Edna.Analysis.FourPL (Params4PL)

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
  , seName :: C f Text
  , seExperimentId :: C f Word32
  , seIsSuspicious :: C f Bool
  , seResult :: C f (PgJSON Params4PL)
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
-- Primary sub-experiment
-------------------------

data PrimarySubExperimentT f = PrimarySubExperimentRec
  { pseExperimentId :: C f Word32
  , pseSubExperimentId :: C f Word32
  } deriving stock Generic
    deriving anyclass Beamable

type PrimarySubExperimentRec = PrimarySubExperimentT Identity

deriving stock instance Show PrimarySubExperimentRec
deriving stock instance Eq PrimarySubExperimentRec

instance Table PrimarySubExperimentT where
  data PrimaryKey PrimarySubExperimentT f = PrimarySubExperimentId (C f Word32) (C f Word32)
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = PrimarySubExperimentId <$> pseExperimentId <*> pseSubExperimentId

deriving stock instance Show (PrimaryKey PrimarySubExperimentT Identity)
deriving stock instance Eq (PrimaryKey PrimarySubExperimentT Identity)

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
