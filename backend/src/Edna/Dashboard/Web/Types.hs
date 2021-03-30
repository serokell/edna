-- | Types that exist specifically for Dashboard API.

module Edna.Dashboard.Web.Types
  ( ExperimentsResp (..)
  , ExperimentResp (..)
  , SubExperimentResp (..)
  , MeasurementResp (..)
  ) where

import Universum

import Data.Aeson.TH (deriveToJSON)
import Data.Swagger (ToSchema(..))
import Data.Time (LocalTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Fmt (Buildable(..), genericF, tupleF, (+|), (|+))
import Servant.Util.Combinators.Logging (ForResponseLog(..), buildForResponse, buildListForResponse)

import Edna.Util
  (CompoundId, IdType(..), MethodologyId, ProjectId, SubExperimentId, TargetId, ednaAesonWebOptions,
  gDeclareNamedSchema, unSqlId)
import Edna.Web.Types (WithId)

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
  , erMethodology :: MethodologyId
  -- ^ Test methodology used in this experiment.
  , erUploadDate :: LocalTime
  -- ^ Date when the experiment was uploaded.
  , erSubExperiments :: [SubExperimentId]
  -- ^ IDs of all sub-experiments from this experiment.
  -- Usually their number is small (≤2, maybe 3).
  } deriving stock (Generic, Show)

instance Buildable ExperimentResp where
  build ExperimentResp {..} =
    "Project " +| erProject |+ ", compound " +| erCompound |+ ", target " +| erTarget |+
    ", methodology " +| erMethodology |+ ", upload date: " +| iso8601Show erUploadDate |+
    ", sub-experiments: " +| map unSqlId erSubExperiments |+ ""

instance Buildable (ForResponseLog ExperimentResp) where
  build = buildForResponse

-- | SubExperiment as response from the server.
data SubExperimentResp = SubExperimentResp
  { serName :: Text
  -- ^ Sub-eperiment name.
  , serIsDefault :: Bool
  -- ^ Whether this subexperiment is the default one.
  , serIsSuspicious :: Bool
  -- ^ Whether this subexperiment's data is suspicious (potentially has
  -- incorrect points).
  , serIC50 :: Double
  -- ^ IC50 computed for this sub-experiment.
  } deriving stock (Generic, Show)

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
  } deriving stock (Generic, Show)

instance Buildable MeasurementResp where
  build MeasurementResp {..} = tupleF (mrConcentration, mrSignal)

instance Buildable (ForResponseLog MeasurementResp) where
  build = buildForResponse

instance Buildable (ForResponseLog [MeasurementResp]) where
  build = buildListForResponse (take 5)

deriveToJSON ednaAesonWebOptions ''ExperimentsResp
deriveToJSON ednaAesonWebOptions ''ExperimentResp
deriveToJSON ednaAesonWebOptions ''SubExperimentResp
deriveToJSON ednaAesonWebOptions ''MeasurementResp

instance ToSchema ExperimentsResp where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema ExperimentResp where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema SubExperimentResp where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema MeasurementResp where
  declareNamedSchema = gDeclareNamedSchema
