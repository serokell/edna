-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Types that exist specifically for Dashboard API.

module Edna.Dashboard.Web.Types
  ( NewSubExperimentReq (..)
  , ExperimentsResp (..)
  , ExperimentResp (..)
  , SubExperimentResp (..)
  , MeasurementResp (..)
  , ExperimentMetadata (..)
  , ExperimentFileBlob (..)
  ) where

import Universum

import Data.Aeson.TH (deriveJSON, deriveToJSON)
import Data.Swagger (NamedSchema(..), ToSchema(..), binarySchema)
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Fmt (Buildable(..), Builder, genericF, tupleF, (+|), (|+))
import Servant.API.ContentTypes (MimeRender, OctetStream)
import Servant.Util.Combinators.Logging (ForResponseLog(..), buildForResponse, buildListForResponse)

import Edna.Analysis.FourPL (AnalysisResult)
import Edna.Util
  (CompoundId, IdType(..), MeasurementId, MethodologyId, ProjectId, SubExperimentId, TargetId,
  ednaAesonWebOptions, gDeclareNamedSchema, unSqlId)
import Edna.Web.Types (WithId)

-- | Data submitted in body to create a new sub-experiment.
data NewSubExperimentReq = NewSubExperimentReq
  { nserName :: Text
  -- ^ Name of the new sub-experiment.
  , nserChanges :: HashSet MeasurementId
  -- ^ This list of measurements specifies which points are enabled/disabled.
  -- If a point is disabled in the existing sub-experiment,
  -- it will be enabled in the new one, and vice versa.
  } deriving stock (Generic, Show)

instance Buildable NewSubExperimentReq where
  build NewSubExperimentReq {..} =
    "new sub-experiment name: " +| nserName |+
    ", changes: " +| toList nserChanges |+ ""

-- | Experiment as response from the server.
data ExperimentsResp = ExperimentsResp
  { erExperiments :: [WithId 'ExperimentId ExperimentResp]
  , erMeanIC50 :: Maybe Double
  -- ^ If compound and target are selected,
  -- we also show mean IC50 for this pair.
  } deriving stock (Generic, Show)

instance Buildable ExperimentsResp where
  build ExperimentsResp {..} =
    "Experiments: " +| erExperiments |+ "\nMean IC50: " +| erMeanIC50 |+ ""

instance Buildable (ForResponseLog ExperimentsResp) where
  build = buildForResponse

-- | Experiment as response from the server.
data ExperimentResp = ExperimentResp
  { erProject :: ProjectId
  -- ^ Project this experiment belongs to.
  , erCompound :: CompoundId
  -- ^ Compound involved in this experiment.
  , erTarget :: TargetId
  -- ^ Compound involved in this experiment.
  , erMethodology :: Maybe MethodologyId
  -- ^ Test methodology used in this experiment.
  , erUploadDate :: UTCTime
  -- ^ Date when the experiment was uploaded.
  , erSubExperiments :: [SubExperimentId]
  -- ^ IDs of all sub-experiments from this experiment.
  -- Usually their number is small (≤2, maybe 3).
  , erPrimarySubExperiment :: SubExperimentId
  -- ^ Idenfitier of the primary sub-experiment from this experiment.
  , erPrimaryIC50 :: Either Text Double
  -- ^ IC50 value computed for the primary sub-experiment. It's provided here
  -- as an optimization. It's possible to get this value by querying sub-experiment
  -- data (since @erPrimarySubExperiment@) is provided, but it would work slower.
  -- @Left err@ may be provided if analysis failed for the primary sub-experiment.
  } deriving stock (Generic, Show)

instance Buildable ExperimentResp where
  build ExperimentResp {..} =
    "Project " +| erProject |+ ", compound " +| erCompound |+ ", target " +| erTarget |+
    ", methodology " +| erMethodology |+ ", upload date: " +| iso8601Show erUploadDate |+
    ", sub-experiments: " +| map unSqlId erSubExperiments |+
    ", primary: " +| erPrimarySubExperiment |+
    ", IC50: " +| either build build erPrimaryIC50 |+ ""

instance Buildable (ForResponseLog ExperimentResp) where
  build = buildForResponse

-- | SubExperiment as response from the server.
data SubExperimentResp = SubExperimentResp
  { serName :: Text
  -- ^ Sub-experiment name.
  , serIsSuspicious :: Bool
  -- ^ Whether this sub-experiment's data is suspicious (potentially has
  -- incorrect points).
  , serResult :: AnalysisResult
  -- ^ 4PL parameters computed for this sub-experiment.
  } deriving stock (Generic, Show, Eq)

instance Buildable SubExperimentResp where
  build = genericF

instance Buildable (ForResponseLog SubExperimentResp) where
  build = buildForResponse

-- | A single experimental measurement from a sub-experiment.
data MeasurementResp = MeasurementResp
  { mrConcentration :: Double
  -- ^ Concentration for which the signal is measured.
  , mrSignal :: Double
  -- ^ Something that is measured.
  , mrIsEnabled :: Bool
  -- ^ Whether this point is enabled.
  } deriving stock (Generic, Show, Eq)

instance Buildable MeasurementResp where
  build MeasurementResp {..}
    | mrIsEnabled = tupleF (mrConcentration, mrSignal)
    | otherwise = tupleF (mrConcentration, mrSignal, "DISABLED" :: Builder)

instance Buildable (ForResponseLog MeasurementResp) where
  build = buildForResponse

instance Buildable (ForResponseLog [MeasurementResp]) where
  build = buildListForResponse (take 5)

-- | All metadata about an experiment.
data ExperimentMetadata = ExperimentMetadata
  { emDescription :: Text
  -- ^ Description provided manually during file upload.
  , emFileMetadata :: [Text]
  -- ^ Metadata stored inside the corresponding file.
  } deriving stock (Generic, Show, Eq)

instance Buildable ExperimentMetadata where
  build = genericF

instance Buildable (ForResponseLog ExperimentMetadata) where
  build = buildForResponse

newtype ExperimentFileBlob = ExperimentFileBlob
  { unExperimentFileBlob :: LByteString
  } deriving stock (Generic, Show, Eq)
    deriving newtype (MimeRender OctetStream)

instance Buildable ExperimentFileBlob where
  build (ExperimentFileBlob blob) =
    build (length blob) <> " bytes of ExperimentFileBlob"

instance Buildable (ForResponseLog ExperimentFileBlob) where
  build = buildForResponse

deriveJSON ednaAesonWebOptions ''NewSubExperimentReq
deriveToJSON ednaAesonWebOptions ''ExperimentsResp
deriveToJSON ednaAesonWebOptions ''ExperimentResp
deriveToJSON ednaAesonWebOptions ''SubExperimentResp
deriveToJSON ednaAesonWebOptions ''MeasurementResp
deriveToJSON ednaAesonWebOptions ''ExperimentMetadata

instance ToSchema NewSubExperimentReq where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema ExperimentsResp where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema ExperimentResp where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema SubExperimentResp where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema MeasurementResp where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema ExperimentMetadata where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema ExperimentFileBlob where
  declareNamedSchema _ =
    pure (NamedSchema (Just "ExperimentFileBlob") binarySchema)
