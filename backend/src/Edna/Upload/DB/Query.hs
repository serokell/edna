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

import Database.Beam.Postgres (PgJSON(..), Postgres)
import Database.Beam.Query (default_, insert, insertExpressions, insertValues, val_)
import Database.Beam.Query.Internal (QExpr)

import Edna.Analysis.FourPL (Params4PLResp(..))
import Edna.DB.Integration (runInsert', runInsertReturningList', runInsertReturningOne')
import Edna.DB.Schema (EdnaSchema(..), ednaSchema)
import Edna.Dashboard.DB.Schema
  (ExperimentT(..), MeasurementT(..), PrimarySubExperimentT(..), RemovedMeasurementsT(..),
  SubExperimentRec, SubExperimentT(..), theOnlyAnalysisMethodId)
import Edna.ExperimentReader.Types as EReader
import Edna.Setup (Edna)
import Edna.Upload.DB.Schema (ExperimentFileT(..))
import Edna.Util
  (CompoundId, ExperimentFileId, ExperimentId, MeasurementId, MethodologyId, ProjectId, SqlId(..),
  SubExperimentId, TargetId, fromSqlSerial)

-- | Insert experiments and return their IDs
-- Returned IDs are sorted by compound and target ids of experiment
insertExperiments :: [(ExperimentFileId, CompoundId, TargetId)] -> Edna [ExperimentId]
insertExperiments experiments = map (fromSqlSerial . eExperimentId) .
  sortWith (\a -> (eCompoundId a, eTargetId a)) <$>
    runInsertReturningList' (insert (esExperiment ednaSchema) $ insertExpressions $
      flip map experiments $ \(SqlId experimentFileId, SqlId compoundId, SqlId targetId) ->
        ExperimentRec
        { eExperimentId = default_
        , eExperimentFileId = val_ experimentFileId
        , eCompoundId = val_ compoundId
        , eTargetId = val_ targetId
        })

-- | Insert sub-experiments and return their IDs
-- Returned IDs are sorted by experiment ids of sub-experiment
insertSubExperiments :: [Params4PLResp] -> Edna [SubExperimentId]
insertSubExperiments params = map (fromSqlSerial . seSubExperimentId) .
  sortWith seExperimentId <$>
    runInsertReturningList' (insert (esSubExperiment ednaSchema) $ insertExpressions $
      flip map params $ \Params4PLResp{plrspExperiment = SqlId experimentId, ..} ->
        SubExperimentRec
        { seSubExperimentId = default_
        , seName = val_ "Primary"
        , seAnalysisMethodId = val_ theOnlyAnalysisMethodId
        , seExperimentId = val_ experimentId
        , seIsSuspicious = val_ False
        , seResult = val_ (PgJSON plrspData)
        })

-- | Insert a new sub-experiment as a child of existing experiment.
insertSubExperiment :: ExperimentId -> Text -> Params4PLResp -> Edna SubExperimentRec
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
      , seResult = val_ $ PgJSON $ plrspData result
      }

-- | Insert given pairs of IDs into the table with primary sub-experiments, i. e.
-- mark sub-experiments with given IDs as the primary one for experiments with
-- given IDs.
insertPrimarySubExperiments :: [(ExperimentId, SubExperimentId)] -> Edna ()
insertPrimarySubExperiments ids = runInsert' (insert (esPrimarySubExperiment ednaSchema) $
  insertValues $ flip map ids $ \(SqlId experimentId, SqlId subExpId) ->
    PrimarySubExperimentRec
    { pseExperimentId = experimentId
    , pseSubExperimentId = subExpId
    })

-- | Insert list of measurements for given experiment and return IDs of these measurements
insertMeasurements :: ExperimentId -> [Measurement] -> Edna [MeasurementId]
insertMeasurements (SqlId expId) measurements = map (fromSqlSerial . mMeasurementId) <$>
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
