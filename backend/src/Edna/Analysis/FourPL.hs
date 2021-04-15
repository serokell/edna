-- | Implementation of 4PL (four parameter logistic) analysis.

module Edna.Analysis.FourPL
  ( Params4PL (..)
  , AnalysisResult
  , Params4PLResp (..)
  , Params4PLReq (..)
  , analyse4PL
  , analyse4PLOne
  ) where

import Universum

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.TH (deriveFromJSON, deriveToJSON)
import Data.Swagger (ToSchema(..))
import Fmt (Buildable(..), genericF, tupleF)
import Servant.Util.Combinators.Logging (ForResponseLog, buildForResponse)

import Edna.Analysis.Infra (PythonError(..), callPythonAnalysis)
import Edna.Setup (Edna)
import Edna.Util (ExperimentId, SqlId(..), ednaAesonPythonOptions)

--------------------------------
-- Params4PL and AnalysisResult
--------------------------------

-- | Parameters of 4PL function, analysis outcome of this analysis method.
-- The function is defined as follows:
-- @f(x) = d + (a - d) / (1 + (x / c)^b)@
--
-- 4 fields of this type are @a@, @b@, @c@ and @d@ parameters respectively.
data Params4PL = Params4PL
  { p4plA :: Double
  , p4plB :: Double
  , p4plC :: Double
  , p4plD :: Double
  } deriving stock (Generic, Show, Eq)

type Params4PLTuple = (Double, Double, Double, Double)

toTuple :: Params4PL -> Params4PLTuple
toTuple Params4PL {..} = (p4plA, p4plB, p4plC, p4plD)

instance Buildable Params4PL where
  build = tupleF . toTuple

instance Buildable (ForResponseLog Params4PL) where
  build = buildForResponse

-- Serializing as a tuple for brevity.
instance ToJSON Params4PL where
  toJSON = toJSON . toTuple
  toEncoding = toEncoding . toTuple

instance FromJSON Params4PL where
  parseJSON = fmap constr . parseJSON
    where
      constr (a, b, c, d) = Params4PL a b c d

instance ToSchema Params4PL where
  declareNamedSchema Proxy =
    declareNamedSchema @Params4PLTuple Proxy

type AnalysisResult = Either Text Params4PL

instance Buildable AnalysisResult where
  build = genericF

instance Buildable (ForResponseLog AnalysisResult) where
  build = buildForResponse

--------------------------------
-- Analysis interface and implementation
--------------------------------

-- | Data submitted for analysis of one experiment.
data Params4PLReq = Params4PLReq
  { plreqExperiment :: ExperimentId
  , plreqFindOutliers :: Bool
  , plreqData :: [(Double, Double)]
  } deriving stock (Generic, Show, Eq)

-- | Data returned by Python code for each experiment.
data Params4PLRespPy = Params4PLRespPy
  { plrspyExperiment :: ExperimentId
  , plrspyStatus :: Text
  , plrspyParams :: Maybe Params4PL
  , plrspyOutliers :: Maybe [Word]
  , plrspyNewParams :: Maybe Params4PL
  } deriving stock (Generic, Show, Eq)

deriveToJSON ednaAesonPythonOptions ''Params4PLReq
deriveFromJSON ednaAesonPythonOptions ''Params4PLRespPy

-- | Data returned from 'analyse4PL' for each experiment.
data Params4PLResp = Params4PLResp
  { plrspParams :: Params4PL
  -- ^ Parameters computed for given experiment.
  , plrspNewSubExp :: Maybe (NonEmpty Word, Params4PL)
  -- ^ Information about optional new sub-experiment: indices of outliers and
  -- new 4PL parameters.
  } deriving stock (Generic, Show, Eq)

-- | This function analyses data points and for each experiment computes
-- the following:
--
-- 1. Best 4PL parameters for given points.
-- 2. If 'plreqFindOutliers' is enabled, it tries to find points that look like
-- outliers.
-- 3. If any outliers are found, it computes 4PL parameters for points without
-- outliers.
--
-- Note: there are two different notions of outliers:
--
-- 1. Some points are explicitly marked as outliers in experiment data files.
-- They are disabled by default.
-- 2. In addition to that, our analysis may automatically detect suspicious points
-- that we treat as "likely outliers".
analyse4PL :: [Params4PLReq] -> Edna [(ExperimentId, Either Text Params4PLResp)]
analyse4PL requests =
  callPythonAnalysis "ic50_analysis.py" requests >>= mapM respPyToResp
  where
    respPyToResp :: Params4PLRespPy -> Edna (ExperimentId, Either Text Params4PLResp)
    respPyToResp Params4PLRespPy{..} = (plrspyExperiment,) <$> case plrspyStatus of
      "DONE" -> do
        plrspParams <- whenNothing plrspyParams $
          throwM $ PyInvalidFormat "no params in successful computation"
        let plrspNewSubExp = (,) <$> (nonEmpty =<< plrspyOutliers) <*> plrspyNewParams
        return $ Right Params4PLResp {..}
      err -> return $ Left err

-- | A simplified version of 'analyse4PL' for analysis of one sub-experiment.
-- It doesn't care about IDs and outliers.
analyse4PLOne :: [(Double, Double)] -> Edna AnalysisResult
analyse4PLOne points = do
  analyse4PL [Params4PLReq (SqlId 1) False points] >>= \case
    [(_, eitherResp)] -> pure $ plrspParams <$> eitherResp
    resps -> throwM . PyInvalidFormat $
      "Unexpected number of responses for single request " <> show (length resps)
