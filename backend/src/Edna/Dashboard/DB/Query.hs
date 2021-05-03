-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Queries for the Dashboard part of the database.

module Edna.Dashboard.DB.Query
  ( makePrimarySubExperiment
  , setNameSubExperiment
  , setIsSuspiciousSubExperiment
  , deleteSubExperiment
  , getExperiments
  , getDescriptionAndMetadata
  , getFileNameAndBlob
  , getSubExperiment
  , getMeasurements
  , getRemovedMeasurements
  , getExperimentId
  ) where

import Universum

import Data.Time (LocalTime)
import Database.Beam.Backend (SqlSerial(..))
import Database.Beam.Postgres (PgJSON(..), Postgres)
import Database.Beam.Postgres.Full (deleteReturning)
import Database.Beam.Query
  (Q, QExpr, aggregate_, all_, as_, cast_, countAll_, guard_, int, join_, just_, leftJoin_, lookup_,
  select, subquery_, update, val_, (&&.), (<-.), (==.))
import Fmt (pretty)
import Servant.Util (HList(..), PaginationSpec, (.*.))
import Servant.Util.Beam.Postgres (sortBy_)
import Servant.Util.Combinators.Sorting.Backend (fieldSort)

import Edna.Analysis.FourPL (AnalysisResult, Params4PL(..))
import Edna.DB.Integration
  (runDeleteReturningList', runSelectReturningList', runSelectReturningOne', runUpdate')
import Edna.DB.Schema (EdnaSchema(..), ednaSchema)
import Edna.DB.Util (groupAndPaginate, sortingSpecWithId)
import Edna.Dashboard.DB.Schema
import Edna.Dashboard.Web.Types (ExperimentResp(..), ExperimentSortingSpec)
import Edna.ExperimentReader.Types (FileMetadata)
import Edna.Library.DB.Schema
  (CompoundRec, CompoundT(..), TargetRec, TargetT(..), TestMethodologyRec, TestMethodologyT(..))
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
-- Returns @True@ iff sub-experiment was successfully deleted (i. e. is known and
-- not primary).
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

-- There can't be more than one test methodology for experiment, so we put it here,
-- even though we are using LEFT JOIN for it.
type ExperimentTupleHelper =
  ( ExperimentRec
  , Word32
  -- Note: instead of having full records here, we probably can have only names,
  -- however, then we will have @Text@ values and it will be harder to figure
  -- out what each @Text@ means.
  -- In EDNA-125 we may add @Name@ newtype and use it here.
  , CompoundRec
  , TargetRec
  , Maybe TestMethodologyRec
  , LocalTime
  , Word32
  )

type SubExperimentTupleHelper = (SqlSerial Word32, PgJSON AnalysisResult)

-- | Get data about all experiments using 3 optional filters: by project ID,
-- compound ID and target ID.
getExperiments :: Maybe ProjectId -> Maybe CompoundId -> Maybe TargetId ->
  ExperimentSortingSpec -> PaginationSpec -> Edna [(ExperimentId, ExperimentResp)]
getExperiments mProj mComp mTarget sorting pagination =
  fmap (
    map convert .
    groupAndPaginate (Just pagination) (unSerial . eExperimentId . view _1) .
    map (\(expTuple, (mId, mResult)) -> (expTuple, (,) <$> mId <*> mResult))
    ) $
  runSelectReturningList' $ select $
  sortBy_ (sortingSpecWithId sorting) sortingApp do
    experiment <- all_ $ esExperiment ednaSchema

    experimentFile <- all_ $ esExperimentFile ednaSchema
    guard_ (eExperimentFileId experiment ==. cast_ (efExperimentFileId experimentFile) int)

    compound <- join_ (esCompound ednaSchema) $ \comp ->
      cast_ (cCompoundId comp) int ==. eCompoundId experiment

    target <- join_ (esTarget ednaSchema) $ \tar ->
      cast_ (tTargetId tar) int ==. eTargetId experiment

    methodology <- leftJoin_ (all_ $ esTestMethodology ednaSchema) $ \testMethod ->
      just_ (cast_ (tmTestMethodologyId testMethod) int) ==. efMethodologyId experimentFile

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
        , compound
        , target
        , methodology
        , efUploadDate experimentFile
        , pseSubExperimentId primarySubExp
        )
      , ( seSubExperimentId subExperiment
        , seResult subExperiment
        )
      )
  where
    sortingApp ((experimentRec, _projectId, compound, target, methodology,
        uploadDate, _subExpId), _) =
      fieldSort @"id" (eExperimentId experimentRec) .*.
      fieldSort @"uploadDate" uploadDate .*.
      fieldSort @"compound" (cName compound) .*.
      fieldSort @"target" (tName target) .*.
      fieldSort @"methodology" (tmName methodology) .*.
      HNil

    convert :: HasCallStack =>
      (ExperimentTupleHelper, [SubExperimentTupleHelper]) ->
      (ExperimentId, ExperimentResp)
    convert ((ExperimentRec {..}, projId, compound, target, mMethodology, uploadDate, primary),
        subExps) =
      ( fromSqlSerial eExperimentId, ExperimentResp
        { erProject = SqlId projId
        , erCompound = (SqlId eCompoundId, cName compound)
        , erTarget = (SqlId eTargetId, tName target)
        , erMethodology =
            (fromSqlSerial . tmTestMethodologyId &&& tmName) <$> mMethodology
        , erUploadDate = localToUTC uploadDate
        , erSubExperiments = map (fromSqlSerial . fst) subExps
        , erPrimarySubExperiment = SqlId primary
        , erPrimaryIC50 = primaryIC50
        }
      )
      where
        -- The total number of sub-experiments for one experiment is expected
        -- to be small, usually it will be less than 4, so linear search should
        -- be completely ok.
        primaryIC50 :: HasCallStack => Either Text Double
        primaryIC50 =
          case find ((SqlSerial primary ==) . fst) subExps of
            Nothing -> error $ "can't find primary sub-experiment: " <> pretty primary
            Just (_, PgJSON analysisResult) -> p4plC <$> analysisResult

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
