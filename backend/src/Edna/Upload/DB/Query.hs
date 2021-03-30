module Edna.Upload.DB.Query
  ( insertExperimentFile
  , insertExperiment
  , insertSubExperiment
  , insertPrimarySubExperiment
  , insertMeasurements
  , insertRemovedMeasurements
  ) where

import Universum

import Database.Beam.Postgres (PgJSON(..), Postgres)
import Database.Beam.Query (default_, insert, insertExpressions, insertValues, val_)
import Database.Beam.Query.Internal (QExpr)

import Edna.DB.Integration (runInsert', runInsertReturningList', runInsertReturningOne')
import Edna.DB.Schema (EdnaSchema(..), ednaSchema)
import Edna.Dashboard.DB.Schema
  (ExperimentT(..), MeasurementT(..), PrimarySubExperimentT(..), RemovedMeasurementsT(..),
  SubExperimentT(..), theOnlyAnalysisMethodId)
import Edna.ExperimentReader.Types as EReader
import Edna.Setup (Edna)
import Edna.Upload.DB.Schema (ExperimentFileT(..))
import Edna.Util
  (CompoundId, ExperimentFileId, ExperimentId, MeasurementId, MethodologyId, ProjectId, SqlId(..),
  SubExperimentId, TargetId, fromSqlSerial)

-- | Insert experiment and return its ID
insertExperiment :: ExperimentFileId -> CompoundId -> TargetId -> Edna ExperimentId
insertExperiment (SqlId experimentFileId) (SqlId compoundId) (SqlId targetId) =
  fromSqlSerial . eExperimentId <$>
    runInsertReturningOne' (insert (esExperiment ednaSchema) $ insertExpressions
      [ ExperimentRec
        { eExperimentId = default_
        , eExperimentFileId = val_ experimentFileId
        , eCompoundId = val_ compoundId
        , eTargetId = val_ targetId
        }
      ])

-- | Insert primary sub-experiment and return its ID
--
-- TODO [EDNA-73] Support non-primary sub-experiments
insertSubExperiment :: ExperimentId -> Edna SubExperimentId
insertSubExperiment (SqlId experimentId) = fromSqlSerial . seSubExperimentId <$>
  runInsertReturningOne' (insert (esSubExperiment ednaSchema) $ insertExpressions
    [ SubExperimentRec
      { seSubExperimentId = default_
      , seName = val_ "Primary"
      , seAnalysisMethodId = val_ theOnlyAnalysisMethodId
      , seExperimentId = val_ experimentId
      , seIsSuspicious = val_ False
      , seResult = val_ (PgJSON 10)  -- stub value, will be computed in EDNA-71
      }
    ])

-- | Insert given pair of IDs into the table with primary sub-experiments, i. e.
-- mark sub-experiment with given ID as the primary one for experiment with
-- given ID.
insertPrimarySubExperiment :: ExperimentId -> SubExperimentId -> Edna ()
insertPrimarySubExperiment (SqlId experimentId) (SqlId subExpId) =
  runInsert' (insert (esPrimarySubExperiment ednaSchema) $
  insertValues
    [ PrimarySubExperimentRec
      { pseExperimentId = experimentId
      , pseSubExperimentId = subExpId
      }
    ])

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
