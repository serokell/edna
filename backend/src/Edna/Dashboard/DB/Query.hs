-- | Queries for the Dashboard part of the database.

module Edna.Dashboard.DB.Query
  ( getExperiments
  , getSubExperiment
  , getMeasurements
  , getRemovedMeasurements
  ) where

import Universum

import qualified Data.List.NonEmpty as NE

import Data.Time (LocalTime)
import Database.Beam.Backend (SqlSerial(..))
import Database.Beam.Query
  (all_, asc_, cast_, guard_, int, leftJoin_, lookup_, orderBy_, select, val_, (==.))

import Edna.DB.Integration (runSelectReturningList', runSelectReturningOne')
import Edna.DB.Schema (EdnaSchema(..), ednaSchema)
import Edna.Dashboard.DB.Schema
import Edna.Dashboard.Web.Types (ExperimentResp(..))
import Edna.Setup (Edna)
import Edna.Upload.DB.Schema (ExperimentFileT(..))
import Edna.Util as U
  (CompoundId, ExperimentId, MeasurementId, ProjectId, SqlId(..), SubExperimentId, TargetId,
  fromSqlSerial, localToUTC)

-- | Get data about all experiments using 3 optional filters: by project ID,
-- compound ID and target ID.
getExperiments :: Maybe ProjectId -> Maybe CompoundId -> Maybe TargetId ->
  Edna [(ExperimentId, ExperimentResp)]
getExperiments mProj mComp mTarget =
  fmap (map convert . groupSubExps) $ runSelectReturningList' $ select $
  orderBy_ (\(tuple, _) -> asc_ $ eExperimentId (tuple ^. _1)) $ do
    experiment <- all_ $ esExperiment ednaSchema

    experimentFile <- all_ $ esExperimentFile ednaSchema
    guard_ (eExperimentFileId experiment ==. cast_ (efExperimentFileId experimentFile) int)

    primarySubExp <- all_ $ esPrimarySubExperiment ednaSchema
    guard_ (pseExperimentId primarySubExp ==. cast_ (eExperimentId experiment) int)

    subExperiment <- leftJoin_ (all_ $ esSubExperiment ednaSchema) $
      \subExp -> cast_ (seExperimentId subExp) int ==. eExperimentId experiment

    whenJust mProj $ \(SqlId projId) ->
      guard_ (efProjectId experimentFile ==. val_ projId)
    whenJust mComp $ \(SqlId compId) ->
      guard_ (eCompoundId experiment ==. val_ compId)
    whenJust mTarget $ \(SqlId targetId) ->
      guard_ (eTargetId experiment ==. val_ targetId)
    -- We don't return the whole experiment file because it contains a large blob.
    return
      ( ( experiment
        , efProjectId experimentFile
        , efMethodologyId experimentFile
        , efUploadDate experimentFile
        , pseSubExperimentId primarySubExp
        )
      , seSubExperimentId subExperiment
      )
  where
    groupSubExps ::
      [((ExperimentRec, Word32, Maybe Word32, LocalTime, Word32), Maybe (SqlSerial Word32))] ->
      [((ExperimentRec, Word32, Maybe Word32, LocalTime, Word32), [SqlSerial Word32])]
    groupSubExps =
      map (\ne -> (fst (head ne), mapMaybe snd (toList ne))) .
      NE.groupBy (\a b -> eExperimentId (a ^. _1 . _1) == eExperimentId (b ^. _1 . _1))

    convert ::
      ((ExperimentRec, Word32, Maybe Word32, LocalTime, Word32), [SqlSerial Word32]) ->
      (ExperimentId, ExperimentResp)
    convert ((ExperimentRec {..}, projId, methodId, uploadDate, primary), subExpIds) =
      ( fromSqlSerial eExperimentId, ExperimentResp
        { erProject = SqlId projId
        , erCompound = SqlId eCompoundId
        , erTarget = SqlId eTargetId
        , erMethodology = SqlId <$> methodId
        , erUploadDate = localToUTC uploadDate
        , erSubExperiments = map fromSqlSerial subExpIds
        , erPrimarySubExperiment = SqlId primary
        }
      )

-- | Get all stored data about sub-experiment with given ID.
getSubExperiment :: SubExperimentId -> Edna (Maybe SubExperimentRec)
getSubExperiment (SqlId subExperimentId) = runSelectReturningOne' $
  lookup_ (esSubExperiment ednaSchema) $ SubExperimentId $ SqlSerial subExperimentId

-- | Get all measurements from the given sub-experiment:
-- both active and inactive (removed).
getMeasurements :: SubExperimentId -> Edna [MeasurementRec]
getMeasurements i = getSubExperiment i >>= \case
  Nothing -> pure []
  Just se -> runSelectReturningList' $ select $ do
    measurement <- all_ $ esMeasurement ednaSchema
    guard_ (mExperimentId measurement ==. val_ (seExperimentId se))
    pure measurement

-- | Get IDs of all measurements deactivated in the sub-experiment with given ID.
getRemovedMeasurements :: SubExperimentId -> Edna [MeasurementId]
getRemovedMeasurements (SqlId subExperimentId) =
  fmap (map (SqlId . rmMeasurementId)) . runSelectReturningList' $ select $ do
    removedMeasurement <- all_ $ esRemovedMeasurements ednaSchema
    guard_ (rmSubExperimentId removedMeasurement ==. val_ subExperimentId)
    pure removedMeasurement
