-- | Implementation of Dashboard functionality

module Edna.Dashboard.Service
  ( -- * Implementation
    getExperiments
  , getSubExperiment
  , getMeasurements
  ) where

import Universum

import qualified Data.HashSet as HS

import Database.Beam.Backend.SQL.BeamExtensions (unSerial)
import Database.Beam.Postgres (PgJSON(..))

import qualified Edna.Dashboard.DB.Query as Q

import Edna.Dashboard.DB.Schema (MeasurementT(..), SubExperimentT(..))
import Edna.Dashboard.Error (DashboardError(..))
import Edna.Dashboard.Web.Types
  (ExperimentResp(..), ExperimentsResp(..), MeasurementResp(..), SubExperimentResp(..))
import Edna.Setup (Edna)
import Edna.Util
  (CompoundId, IdType(..), ProjectId, SubExperimentId, TargetId, justOrThrow, unSqlId)
import Edna.Web.Types (WithId(..))

-- | Get data about all experiments using 3 optional filters: by project ID,
-- compound ID and target ID. If filters by compound and target are specified,
-- also compute average IC50 for them.
getExperiments :: Maybe ProjectId -> Maybe CompoundId -> Maybe TargetId ->
  Edna ExperimentsResp
getExperiments mProj mComp mTarget = do
  pairs <- Q.getExperiments mProj mComp mTarget
  meanIC50 <- case (mComp, mTarget) of
    (Just _, Just _) -> Just <$> computeMeanIC50 (map snd pairs)
    _ -> pure Nothing
  return ExperimentsResp
    { erExperiments = map (uncurry WithId) pairs
    , erMeanIC50 = meanIC50
    }

-- It may be not the most efficient solution to compute it every time,
-- but let's not optimize prematurely.
computeMeanIC50 :: [ExperimentResp] -> Edna Double
computeMeanIC50 resps = do
  avg <$> mapM (getDefaultIC50 . erPrimarySubExperiment) resps
  where
    avg :: [Double] -> Double
    avg items = sum items / fromIntegral (length items)

    getDefaultIC50 :: SubExperimentId -> Edna Double
    getDefaultIC50 subExpId = do
      SubExperimentRec {..} <-
        justOrThrow (DESubExperimentNotFound subExpId) =<<
        Q.getSubExperiment subExpId
      return $ unwrapResult seResult

unwrapResult :: PgJSON Double -> Double
unwrapResult (PgJSON res) = res

-- | Get sub-experiment with given ID.
getSubExperiment :: SubExperimentId -> Edna (WithId 'SubExperimentId SubExperimentResp)
getSubExperiment subExpId = do
  SubExperimentRec {..} <-
    justOrThrow (DESubExperimentNotFound subExpId) =<<
    Q.getSubExperiment subExpId
  return $ WithId subExpId SubExperimentResp
    { serName = seName
    , serIsSuspicious = seIsSuspicious
    , serIC50 = unwrapResult seResult
    }

-- | Get all measurements from sub-experiment with given ID.
getMeasurements :: SubExperimentId -> Edna [MeasurementResp]
getMeasurements subExpId = do
  measurementRecs <- Q.getMeasurements subExpId
  removedSet <- HS.fromList . map unSqlId <$> Q.getRemovedMeasurements subExpId
  let
    convert MeasurementRec {..} = MeasurementResp
      { mrConcentration = mConcentration
      , mrSignal = mSignal
      , mrIsEnabled = not $ unSerial mMeasurementId `HS.member` removedSet
      }
  return (map convert measurementRecs)
