-- | Implementation of Dashboard functionality

module Edna.Dashboard.Service
  ( -- * Implementation
    makePrimarySubExperiment
  , setNameSubExperiment
  , setIsSuspiciousSubExperiment
  , deleteSubExperiment
  , newSubExperiment
  , analyseNewSubExperiment
  , getExperiments
  , getExperimentMetadata
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

import Edna.Analysis.FourPL (Params4PL(..), analyse4PL)
import Edna.DB.Integration (transact)
import Edna.Dashboard.DB.Schema (MeasurementT(..), SubExperimentRec, SubExperimentT(..))
import Edna.Dashboard.Error (DashboardError(..))
import Edna.Dashboard.Web.Types
  (ExperimentMetadata(..), ExperimentResp(..), ExperimentsResp(..), MeasurementResp(..),
  NewSubExperimentReq(..), SubExperimentResp(..))
import Edna.ExperimentReader.Types (FileMetadata(..))
import Edna.Logging (logMessage)
import Edna.Setup (Edna)
import Edna.Upload.DB.Query (insertRemovedMeasurements)
import Edna.Util
  (CompoundId, ExperimentId, IdType(..), MeasurementId, ProjectId, SubExperimentId, TargetId,
  fromSqlSerial, justOrThrow, unSqlId)
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

-- | Create a new sub-experiment based on existing one.
newSubExperiment ::
  SubExperimentId -> NewSubExperimentReq -> Edna (WithId 'SubExperimentId SubExperimentResp)
newSubExperiment subExpId req = do
  (removed, newResult) <- analyseNewSubExperiment subExpId req
  transact $ do
    expId <-
      justOrThrow (DESubExperimentNotFound subExpId) =<< Q.getExperimentId subExpId
    result <- subExperimentRecToResp <$>
      Q.createSubExperiment expId (nserName req) newResult
    result <$ insertRemovedMeasurements (wiId result) removed

-- | A version of 'newSubExperiment' that doesn't save anything to the DB.
-- Returns IDs of all removed measurements in the new sub-experiment and
-- 4PL parameters.
analyseNewSubExperiment ::
  SubExperimentId -> NewSubExperimentReq -> Edna ([MeasurementId], Params4PL)
analyseNewSubExperiment subExpId NewSubExperimentReq {..} = do
  measurements <- getMeasurements subExpId
  liftIO $ (computeRemovedMeasurements measurements,) <$>
    analyse4PL (computeNewPoints measurements)
  where
    computeNewPoints ::
      [WithId 'MeasurementId MeasurementResp] -> [(Double, Double)]
    computeNewPoints = mapMaybe stepActive

    stepActive ::
      WithId 'MeasurementId MeasurementResp -> Maybe (Double, Double)
    stepActive wi@WithId {..} =
      (mrConcentration wItem, mrSignal wItem) <$ guard (not $ isRemoved wi)

    computeRemovedMeasurements ::
      [WithId 'MeasurementId MeasurementResp] -> [MeasurementId]
    computeRemovedMeasurements = mapMaybe stepRemoved

    stepRemoved ::
      WithId 'MeasurementId MeasurementResp -> Maybe MeasurementId
    stepRemoved wi = wiId wi <$ guard (isRemoved wi)

    isRemoved ::
      WithId 'MeasurementId MeasurementResp -> Bool
    isRemoved WithId {..} = mrIsEnabled wItem == HS.member wiId nserChanges

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
      return $ p4plC $ unwrapResult seResult

unwrapResult :: PgJSON Params4PL -> Params4PL
unwrapResult (PgJSON res) = res

-- | Get all metadata about experiment data file containing experiment
-- with this ID. "All" metadata means metadata from the file itself
-- along with description provided by the user.
getExperimentMetadata :: ExperimentId -> Edna ExperimentMetadata
getExperimentMetadata expId =
  Q.getDescriptionAndMetadata expId >>=
  justOrThrow (DEExperimentNotFound expId) <&>
  \(description, PgJSON (FileMetadata meta)) -> ExperimentMetadata description meta

-- | Get sub-experiment with given ID.
getSubExperiment :: SubExperimentId -> Edna (WithId 'SubExperimentId SubExperimentResp)
getSubExperiment subExpId =
  justOrThrow (DESubExperimentNotFound subExpId) =<<
  subExperimentRecToResp <<$>> Q.getSubExperiment subExpId

subExperimentRecToResp ::
  SubExperimentRec -> WithId 'SubExperimentId SubExperimentResp
subExperimentRecToResp SubExperimentRec {..} =
  WithId (fromSqlSerial seSubExperimentId) SubExperimentResp
    { serName = seName
    , serIsSuspicious = seIsSuspicious
    , serResult = unwrapResult seResult
    }

-- | Get all measurements from sub-experiment with given ID.
getMeasurements :: SubExperimentId -> Edna [WithId 'MeasurementId MeasurementResp]
getMeasurements subExpId = do
  expId <-
    justOrThrow (DESubExperimentNotFound subExpId) =<< Q.getExperimentId subExpId
  measurementRecs <- Q.getMeasurements expId
  removedSet <- HS.fromList . map unSqlId <$> Q.getRemovedMeasurements subExpId
  let
    convert MeasurementRec {..} =
      WithId (fromSqlSerial mMeasurementId) $ MeasurementResp
      { mrConcentration = mConcentration
      , mrSignal = mSignal
      , mrIsEnabled = not $ unSerial mMeasurementId `HS.member` removedSet
      }
  return (map convert measurementRecs)
