-- | Queries for the Dashboard part of the database.

module Edna.Dashboard.DB.Query
  ( makePrimarySubExperiment
  , setNameSubExperiment
  , setIsSuspiciousSubExperiment
  , deleteSubExperiment
  , createSubExperiment
  , getExperiments
  , getDescriptionAndMetadata
  , getFileNameAndBlob
  , getSubExperiment
  , getMeasurements
  , getRemovedMeasurements
  , getExperimentId
  ) where

import Universum

import qualified Data.List.NonEmpty as NE

import Data.Time (LocalTime)
import Database.Beam.Backend (SqlSerial(..))
import Database.Beam.Postgres (PgJSON(..), Postgres)
import Database.Beam.Postgres.Full (deleteReturning)
import Database.Beam.Query
  (Q, QExpr, aggregate_, all_, as_, asc_, cast_, countAll_, default_, guard_, insert,
  insertExpressions, int, leftJoin_, lookup_, orderBy_, select, subquery_, update, val_, (&&.),
  (<-.), (==.))

import Edna.Analysis.FourPL (Params4PL)
import Edna.DB.Integration
  (runDeleteReturningList', runInsertReturningOne', runSelectReturningList', runSelectReturningOne',
  runUpdate')
import Edna.DB.Schema (EdnaSchema(..), ednaSchema)
import Edna.Dashboard.DB.Schema
import Edna.Dashboard.Web.Types (ExperimentResp(..))
import Edna.ExperimentReader.Types (FileMetadata)
import Edna.Setup (Edna)
import Edna.Upload.DB.Schema (ExperimentFileT(..))
import Edna.Util as U
  (CompoundId, ExperimentId, MeasurementId, ProjectId, SqlId(..), SubExperimentId, TargetId,
  fromSqlSerial, localToUTC)

-- | Make given sub-experiment the primary one for its parent experiment.
-- Previous primary sub-experiment is no longer primary.
makePrimarySubExperiment :: SubExperimentId -> Edna ()
makePrimarySubExperiment (SqlId subExpId) =
  runUpdate' $ update (esPrimarySubExperiment ednaSchema)
  (\pse -> pseSubExperimentId pse <-. val_ subExpId)
  (\pse -> pseExperimentId pse ==. subquery_ (do
    subExperiment <- all_ (esSubExperiment ednaSchema)
    guard_ (seSubExperimentId subExperiment ==. val_ (SqlSerial subExpId))
    return (seExperimentId subExperiment)
    ))

-- | Update name of a sub-experiment.
setNameSubExperiment :: SubExperimentId -> Text -> Edna ()
setNameSubExperiment (SqlId subExpId) name =
  runUpdate' $ update (esSubExperiment ednaSchema)
  (\se -> seName se <-. val_ name)
  (\se -> seSubExperimentId se ==. val_ (SqlSerial subExpId))

-- | Update @isSuspicious@ flag for a sub-experiment.
setIsSuspiciousSubExperiment :: SubExperimentId -> Bool -> Edna ()
setIsSuspiciousSubExperiment (SqlId subExpId) isSuspicious =
  runUpdate' $ update (esSubExperiment ednaSchema)
  (\se -> seIsSuspicious se <-. val_ isSuspicious)
  (\se -> seSubExperimentId se ==. val_ (SqlSerial subExpId))

-- | Delete a sub-experiment with given ID along with all @removed_measurement@
-- entries for this sub-experiment.
--
-- Does not delete primary sub-experiments.
-- Returns @True@ iff sub-experiment was successfully deleted (i. e. is not
-- primary).
deleteSubExperiment :: SubExperimentId -> Edna Bool
deleteSubExperiment (SqlId subExpId) =
  fmap (not . null) $ runDeleteReturningList' $
  deleteReturning (esSubExperiment ednaSchema) (
  \se -> seSubExperimentId se ==. val_ (SqlSerial subExpId) &&.
     (0 ==. subquery_ (aggregate_ (\_ -> as_ @Int32 countAll_) $ do
       primarySubExp <- all_ (esPrimarySubExperiment ednaSchema)
       guard_ (pseSubExperimentId primarySubExp ==. val_ subExpId)
       pure primarySubExp)
     )
  ) seSubExperimentId

-- | Create a new sub-experiment as a sibling of existing one (sharing
-- given experiment ID).
createSubExperiment :: ExperimentId -> Text -> Params4PL -> Edna SubExperimentRec
createSubExperiment (SqlId experimentId) newName result =
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
      , seResult = val_ (PgJSON result)
      }

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

-- | Get description and metadata of experiment data file storing experiment
-- with this ID.
getDescriptionAndMetadata ::
  ExperimentId -> Edna (Maybe (Text, PgJSON FileMetadata))
getDescriptionAndMetadata expId = runSelectReturningOne' $ select $ do
  experimentFile <- getExperimentFile expId
  return (efDescription experimentFile, efMeta experimentFile)

-- | Get name of the file with experiment along with its binary contents.
getFileNameAndBlob :: ExperimentId -> Edna (Maybe (Text, LByteString))
getFileNameAndBlob expId = runSelectReturningOne' $ select $ do
  experimentFile <- getExperimentFile expId
  return (efName experimentFile, efContents experimentFile)

getExperimentFile :: ExperimentId -> Q Postgres EdnaSchema s $ ExperimentFileT $ QExpr Postgres s
getExperimentFile (SqlId expId) = do
  experiment <- all_ $ esExperiment ednaSchema
  guard_ (eExperimentId experiment ==. val_ (SqlSerial expId))
  experimentFile <- all_ $ esExperimentFile ednaSchema
  guard_ (eExperimentFileId experiment ==. cast_ (efExperimentFileId experimentFile) int)
  return experimentFile

-- | Get all stored data about sub-experiment with given ID.
getSubExperiment :: SubExperimentId -> Edna (Maybe SubExperimentRec)
getSubExperiment (SqlId subExperimentId) = runSelectReturningOne' $
  lookup_ (esSubExperiment ednaSchema) $ SubExperimentId $ SqlSerial subExperimentId

-- | Get all measurements from the given sub-experiment:
-- both active and inactive (removed).
getMeasurements :: ExperimentId -> Edna [MeasurementRec]
getMeasurements (SqlId experimentId) = runSelectReturningList' $ select $ do
  measurement <- all_ $ esMeasurement ednaSchema
  guard_ (mExperimentId measurement ==. val_ experimentId)
  pure measurement

-- | Get IDs of all measurements deactivated in the sub-experiment with given ID.
getRemovedMeasurements :: SubExperimentId -> Edna [MeasurementId]
getRemovedMeasurements (SqlId subExperimentId) =
  fmap (map (SqlId . rmMeasurementId)) . runSelectReturningList' $ select $ do
    removedMeasurement <- all_ $ esRemovedMeasurements ednaSchema
    guard_ (rmSubExperimentId removedMeasurement ==. val_ subExperimentId)
    pure removedMeasurement

-- | Get ID of experiment that stores sub-experiment with given ID.
getExperimentId :: SubExperimentId -> Edna (Maybe ExperimentId)
getExperimentId (SqlId subExpId) =
  (SqlId <<$>>) . runSelectReturningOne' $ select $ do
    subExperiment <- all_ $ esSubExperiment ednaSchema
    guard_ (seSubExperimentId subExperiment ==. val_ (SqlSerial subExpId))
    pure (seExperimentId subExperiment)
