module Edna.Upload.DB.Query
  ( insertExperimentFile
  , insertMeasurements
  , insertRemovedMeasurements
  , insertExperiments
  , insertSubExperiments
  , insertPrimarySubExperiments
  , insertSubExperiment
  ) where

import Universum

import qualified Data.List.NonEmpty as NE

import Database.Beam.Backend (unSerial)
import Database.Beam.Postgres (PgJSON(..), Postgres)
import Database.Beam.Query (default_, insert, insertExpressions, insertValues, val_)
import Database.Beam.Query.Internal (QExpr)

import Edna.Analysis.FourPL (AnalysisResult, Params4PLResp(..))
import Edna.DB.Integration (runInsert', runInsertReturningList', runInsertReturningOne')
import Edna.DB.Schema (EdnaSchema(..), ednaSchema)
import Edna.Dashboard.DB.Schema
  (ExperimentT(..), MeasurementRec, MeasurementT(..), PrimarySubExperimentT(..),
  RemovedMeasurementsT(..), SubExperimentRec, SubExperimentT(..), theOnlyAnalysisMethodId)
import Edna.ExperimentReader.Types as EReader
import Edna.Setup (Edna)
import Edna.Upload.DB.Schema (ExperimentFileT(..))
import Edna.Util
  (CompoundId, ExperimentFileId, ExperimentId, MeasurementId, MethodologyId, ProjectId, SqlId(..),
  SubExperimentId, TargetId, fromSqlSerial)

-- | Insert experiments and return their IDs
-- Returned IDs are sorted by compound and target ids of experiment
insertExperiments ::
  ExperimentFileId -> [(CompoundId, TargetId)] -> Edna [ExperimentId]
insertExperiments (SqlId experimentFileId) experiments =
  map (fromSqlSerial . eExperimentId) .
  sortWith (\a -> (eCompoundId a, eTargetId a)) <$>
    runInsertReturningList' (insert (esExperiment ednaSchema) $ insertExpressions $
      flip map experiments $ \(SqlId compoundId, SqlId targetId) ->
        ExperimentRec
        { eExperimentId = default_
        , eExperimentFileId = val_ experimentFileId
        , eCompoundId = val_ compoundId
        , eTargetId = val_ targetId
        })

-- | Insert sub-experiments and return their IDs.
-- Returned records are grouped and sorted by experiment ids of sub-experiment.
-- For each experiment one or two sub-experiments is inserted, depending on the
-- 'plrspNewSubExp' field. Primary sub-experiment is always in the head.
insertSubExperiments ::
  [(ExperimentId, Either Text Params4PLResp)] -> Edna [NonEmpty SubExperimentRec]
insertSubExperiments params =
  -- If name is 'primaryName', the comparison will return 'False' which is less
  -- than 'True', so primary goes first.
  map (NE.sortWith (\se -> seName se /= primaryName)) .
  NE.groupAllWith seExperimentId <$>
  runInsertReturningList' (insert (esSubExperiment ednaSchema) $ insertExpressions $
    flip foldMap params $ \(SqlId experimentId, eitherResp) ->
      let se name result = SubExperimentRec
            { seSubExperimentId = default_
            , seName = val_ name
            , seAnalysisMethodId = val_ theOnlyAnalysisMethodId
            , seExperimentId = val_ experimentId
            , seIsSuspicious = val_ False
            , seResult = val_ (PgJSON result)
            }
      in catMaybes
      [ Just $ se primaryName $ plrspParams <$> eitherResp
      , (rightToMaybe eitherResp >>= plrspNewSubExp) <&>
        \(_outliers, params4PL) -> se autoName $ Right params4PL
      ])
  where
    autoName = "Automatic"
    primaryName = "Primary"

-- | Insert a new sub-experiment as a child of existing experiment.
insertSubExperiment :: ExperimentId -> Text -> AnalysisResult -> Edna SubExperimentRec
insertSubExperiment (SqlId experimentId) newName result =
  runInsertReturningOne'
    (insert (esSubExperiment ednaSchema) $ insertExpressions [newSubExpRec])
  where
    newSubExpRec :: SubExperimentT (QExpr Postgres s)
    newSubExpRec = SubExperimentRec
      { seSubExperimentId = default_
      , seAnalysisMethodId = val_ theOnlyAnalysisMethodId
      , seName = val_ newName
      , seExperimentId = val_ experimentId
      , seIsSuspicious = val_ False
      , seResult = val_ $ PgJSON result
      }

-- | Insert pairs of IDs from given records into the table with primary sub-experiments,
-- i. e. mark sub-experiments with given IDs as the primary ones for the corresponding experiments.
insertPrimarySubExperiments :: [SubExperimentRec] -> Edna ()
insertPrimarySubExperiments recs = runInsert' (insert (esPrimarySubExperiment ednaSchema) $
  insertValues $ flip map recs $ \SubExperimentRec {..} ->
    PrimarySubExperimentRec
    { pseExperimentId = seExperimentId
    , pseSubExperimentId = unSerial seSubExperimentId
    })

-- | Insert list of measurements for given experiment and return IDs of these measurements
insertMeasurements :: ExperimentId -> [Measurement] -> Edna [MeasurementRec]
insertMeasurements (SqlId expId) measurements =
  runInsertReturningList' (insert (esMeasurement ednaSchema) $ insertExpressions $
    flip map measurements $ \measurement ->
      MeasurementRec
      { mMeasurementId = default_
      , mExperimentId = val_ expId
      , mConcentration = val_ (EReader.mConcentration measurement)
      , mSignal = val_ (EReader.mSignal measurement)
      , mIsOutlier = val_ (EReader.mIsOutlier measurement)
      })

-- | Insert removed measurements for given sub-experiment
insertRemovedMeasurements :: SubExperimentId -> [MeasurementId] -> Edna ()
insertRemovedMeasurements (SqlId subExpId) removedIds =
  runInsert' $ insert (esRemovedMeasurements ednaSchema) $ insertValues $
    flip map removedIds $ \(SqlId removedId) -> RemovedMeasurementsRec subExpId removedId

-- | Insert experiment file and return its ID
insertExperimentFile ::
  ProjectId -> MethodologyId -> FileMetadata -> Text -> Text -> LByteString -> Edna ExperimentFileId
insertExperimentFile (SqlId projId) (SqlId methodId) meta descr fileName blob = do
  fromSqlSerial . efExperimentFileId <$> runInsertReturningOne'
    (insert (esExperimentFile ednaSchema) $ insertExpressions [experimentFileRec])
  where
    experimentFileRec :: ExperimentFileT (QExpr Postgres s)
    experimentFileRec = ExperimentFileRec
      { efExperimentFileId = default_
      , efProjectId = val_ projId
      , efMethodologyId = val_ (Just methodId)
      , efUploadDate = default_
      , efMeta = val_ (PgJSON meta)
      , efDescription = val_ descr
      , efName = val_ fileName
      , efContents = val_ blob
      }
