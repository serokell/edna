-- | Implementation of Dashboard functionality

module Edna.Dashboard.Service
  ( -- * Implementation
    makePrimarySubExperiment
  , setNameSubExperiment
  , setIsSuspiciousSubExperiment
  , deleteSubExperiment
  , getExperiments
  , getSubExperiment
  , getMeasurements
  ) where

import Universum

import qualified Data.HashSet as HS

import Database.Beam.Backend.SQL.BeamExtensions (unSerial)
import Database.Beam.Postgres (PgJSON(..))
import Fmt (fmt, (+|), (|+))
import Servant.API (NoContent(..))

import qualified Edna.Dashboard.DB.Query as Q

import Edna.Dashboard.DB.Schema (MeasurementT(..), SubExperimentT(..))
import Edna.Dashboard.Error (DashboardError(..))
import Edna.Dashboard.Web.Types
  (ExperimentResp(..), ExperimentsResp(..), MeasurementResp(..), SubExperimentResp(..))
import Edna.Logging (logMessage)
import Edna.Setup (Edna)
import Edna.Util
  (CompoundId, IdType(..), ProjectId, SubExperimentId, TargetId, justOrThrow, unSqlId)
import Edna.Web.Types (WithId(..))

-- | Make given sub-experiment the primary one for its parent experiment.
-- Previous primary sub-experiment is no longer primary.
makePrimarySubExperiment :: SubExperimentId ->
  Edna (WithId 'SubExperimentId SubExperimentResp)
makePrimarySubExperiment subExpId = do
  logMessage $ fmt $ "Making sub-experiment " +| subExpId |+ " primary"
  Q.makePrimarySubExperiment subExpId
  getSubExperiment subExpId

-- | Update name of a sub-experiment.
setNameSubExperiment :: SubExperimentId -> Text ->
  Edna (WithId 'SubExperimentId SubExperimentResp)
setNameSubExperiment subExpId name = do
  logMessage $ fmt $ "Setting the name of sub-experiment " +| subExpId |+
    " to " +| name |+ ""
  Q.setNameSubExperiment subExpId name
  getSubExperiment subExpId

-- | Update @isSuspicious@ flag for a sub-experiment.
setIsSuspiciousSubExperiment :: SubExperimentId -> Bool ->
  Edna (WithId 'SubExperimentId SubExperimentResp)
setIsSuspiciousSubExperiment subExpId isSuspicious = do
  logMessage $ fmt $ "Marking sub-experiment " +| subExpId |+ mappend " as "
    if isSuspicious then "suspicious" else "not suspicious"
  Q.setIsSuspiciousSubExperiment subExpId isSuspicious
  getSubExperiment subExpId

-- | Delete a sub-experiment with given ID. Fails if this sub-experiment is
-- primary.
deleteSubExperiment :: SubExperimentId -> Edna NoContent
deleteSubExperiment subExpId = NoContent <$ do
  logMessage $ fmt $ "Deleting sub-experiment " +| subExpId |+ ""
  unlessM (Q.deleteSubExperiment subExpId) $
    throwM $ DECantDeletePrimary subExpId

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
