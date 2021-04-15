-- | Implementation of file upload functionality

module Edna.Upload.Service
  ( parseFile
  , uploadFile
  , UploadError (..)

  -- * Exported for tests
  , parseFile'
  , uploadFile'
  ) where

import Universum

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS

import Fmt ((+|), (|+))
import Lens.Micro.Platform (at, (?~))

import qualified Edna.Library.DB.Query as LQ
import qualified Edna.Upload.DB.Query as UQ

import Edna.Analysis.FourPL (Params4PLReq(..), Params4PLResp(..), analyse4PL)
import Edna.DB.Integration (transact)
import Edna.Dashboard.DB.Schema
  (MeasurementRec, MeasurementT(..), SubExperimentRec, SubExperimentT(..))
import Edna.ExperimentReader.Parser (parseExperimentXls)
import Edna.ExperimentReader.Types as EReader
import Edna.Library.DB.Query (getMethodologyById, getProjectById)
import Edna.Library.DB.Schema as LDB
import Edna.Logging (logDebug, logMessage)
import Edna.Setup
import Edna.Upload.Error (UploadError(..))
import Edna.Upload.Web.Types (FileSummary(..), FileSummaryItem(..), NameAndId(..), sortFileSummary)
import Edna.Util
  (CompoundId, ExperimentFileId, ExperimentId, MeasurementId, MethodologyId, ProjectId, SqlId(..),
  TargetId, fromSqlSerial, justOrThrow, uncurry3)

-- | Parse contents of an experiment data file and return as 'FileSummary'.
-- Uses database to determine which targets are new.
parseFile :: LByteString -> Edna FileSummary
parseFile content =
  parseFile' =<<
  either throwM pure (parseExperimentXls content)

-- | Testable version of 'parseFile'. It takes an already parsed file
-- and this is more convenient for parsing because it's hard to generate
-- experiment data files in tests, but not hard to generate 'FileContents'.
parseFile' :: FileContents -> Edna FileSummary
parseFile' = measurementsToSummary . fcMeasurements

compoundNameToId :: Text -> Edna (Maybe CompoundId)
compoundNameToId compoundName =
  (fromSqlSerial . cCompoundId) <<$>> LQ.getCompoundByName compoundName

targetNameToId :: Text -> Edna (Maybe TargetId)
targetNameToId targetName =
  (fromSqlSerial . tTargetId) <<$>> LQ.getTargetByName targetName

measurementsToSummary :: Map Text TargetMeasurements -> Edna FileSummary
measurementsToSummary =
  fmap (sortFileSummary . FileSummary . toList) . foldM step mempty . toPairs
  where
    step ::
      Map Text FileSummaryItem -> (Text, TargetMeasurements) ->
      Edna $ Map Text FileSummaryItem
    step acc (targetName, TargetMeasurements targetMeasurements) = do
      target <- NameAndId targetName <$> targetNameToId targetName
      compounds <- forM (keys targetMeasurements) $ \compoundName ->
        NameAndId compoundName <$> compoundNameToId compoundName
      return $ acc & at targetName ?~ FileSummaryItem
        { fsiTarget = target
        , fsiCompounds = compounds
        }

-- | Parse an experiment data file and save it to DB.
uploadFile ::
  ProjectId -> MethodologyId -> Text -> Text -> LByteString ->
  Edna FileSummary
uploadFile proj methodology description fileName content = do
  uploadFile' proj methodology description fileName content =<<
    either throwM pure (parseExperimentXls content)

uploadFile' ::
  ProjectId -> MethodologyId -> Text -> Text -> LByteString ->
  FileContents -> Edna FileSummary
uploadFile' projSqlId@(SqlId proj) methodSqlId@(SqlId method)
  description fileName fileBytes fc = do
    logDebug $ "Checking whether project ID " +| proj |+ " exists"
    _ <- getProjectById projSqlId >>= justOrThrow (UEUnknownProject projSqlId)
    logDebug $ "Checking whether test methodology ID " +| method |+ " exists"
    _ <- getMethodologyById methodSqlId >>= justOrThrow (UEUnknownTestMethodology methodSqlId)
    logMessage "A new file is being added to the database along with its data"
    transact $ insertAll projSqlId methodSqlId
  where
    insertAll :: HasCallStack => ProjectId -> MethodologyId -> Edna FileSummary
    insertAll projId methodId = do
      let fileMeasurements = fcMeasurements fc
      targetToId <- HM.fromList <$> mapM insertTarget (keys fileMeasurements)
      let compounds = hashNub $ flip concatMap (elems fileMeasurements) $
            \(TargetMeasurements targetMeasurements) -> keys targetMeasurements
      compoundToId <- HM.fromList <$> mapM insertCompound compounds

      LQ.touchProject projId

      expFileId <- UQ.insertExperimentFile projId methodId (fcMetadata fc)
        description fileName fileBytes

      experiments <- sortWith (\a -> (a ^. _1 . _2, a ^. _1 . _3)) <$>
        (concat <$> (forM (toPairs fileMeasurements) $
          \(targetName, TargetMeasurements targetMeasurements) ->
            forM (toPairs targetMeasurements) $ \(compoundName, measurements) -> do
              let
                getId :: HasCallStack => Text -> HashMap Text (SqlId a) -> SqlId a
                getId name =
                  fromMaybe (error $ "unexpected name: " <> name) .
                  view (at name)
              let targetId = getId targetName targetToId
              let compoundId = getId compoundName compoundToId
              pure ((expFileId, compoundId, targetId), measurements)))

      insertExperiments experiments
      measurementsToSummary fileMeasurements

insertTarget :: Text -> Edna (Text, TargetId)
insertTarget targetName = (targetName,) . fromSqlSerial . tTargetId <$> LQ.insertTarget targetName

insertCompound :: Text -> Edna (Text, CompoundId)
insertCompound compoundName =
  (compoundName,) . fromSqlSerial . cCompoundId <$> LQ.insertCompound compoundName

-- Expects tuples sorted by @(CompoundId, TargetId)@ pairs.
insertExperiments :: [((ExperimentFileId, CompoundId, TargetId), [Measurement])] -> Edna ()
insertExperiments experiments = do
  expIds <- UQ.insertExperiments $ map fst experiments
  let
    -- Sorted by experiment IDs
    measurementsWithIds :: [(ExperimentId, [Measurement])]
    -- The order of IDs and tuples is supposed to match because both are sorted by
    -- @(CompoundId, TargetId) pairs.
    measurementsWithIds = sortWith fst $ zip expIds $ map snd experiments

  -- Sorted by experiment IDs.
  -- While 'analyse4PL' most likely preserves the order, it seems safer to sort
  -- items explicitly here to have less assumptions on Python code.
  analysisResults <- fmap (sortWith fst) $ analyse4PL $
    flip map measurementsWithIds $ \(eId, ms) ->
    Params4PLReq
    { plreqExperiment = eId
    , plreqFindOutliers = True
    , plreqData =
        map (\m -> (EReader.mConcentration m, EReader.mSignal m)) $
          filter (not . EReader.mIsOutlier) ms
    }
  -- Sorted by experiment IDs
  subExps <- UQ.insertSubExperiments analysisResults
  UQ.insertPrimarySubExperiments $ map head subExps
  mapM_ (uncurry3 insertMeasurements)
    (zip3 subExps (map snd measurementsWithIds) (map snd analysisResults))

-- There are 1 or 2 sub-experiments from the same experiment, the head one is always primary.
-- First we insert all measurements to experiment. Then we insert removed measurements:
--
-- * For the primary one removed measurements are all measurements for which
-- @mIsOutlier@ is @True@.
-- * The second one must be provided iff @eitherResp@ holds @plrspNewSubExp@ inside of it.
-- In this case we compute outliers from indices inside @plrspNewSubExp@.
insertMeasurements ::
  HasCallStack =>
  NonEmpty SubExperimentRec -> [Measurement] -> Either Text Params4PLResp -> Edna ()
insertMeasurements subExps measurements eitherResp = do
  let primarySubExp = head subExps
  let expId = SqlId $ seExperimentId primarySubExp
  -- Insert measurements (trivial)
  measurementRecs <- UQ.insertMeasurements expId measurements
  -- Compute and insert removed measurements for the primary sub-experiment.
  -- We simply check @mIsOutlier@ here.
  let primarySubExpId = fromSqlSerial $ seSubExperimentId primarySubExp
  let primaryRemovedIds =
        mapMaybe (\MeasurementRec {..} -> fromSqlSerial mMeasurementId <$ guard mIsOutlier)
        measurementRecs
  UQ.insertRemovedMeasurements primarySubExpId primaryRemovedIds
  -- If 2 sub-experiments are passed to this function and @plrspNewSubExp@ is available,
  -- compute removed measurements for the secondary sub-experiment.
  case (toList subExps, rightToMaybe eitherResp >>= plrspNewSubExp) of
    ([_], Nothing) -> pass
    ([_primary, autoSubExp], Just (outliers, _)) ->
      let removed = autoRemovedIds measurementRecs outliers
      in UQ.insertRemovedMeasurements (fromSqlSerial $ seSubExperimentId autoSubExp) removed
    _ -> error $
      "unexpected combination of subExps and eitherResp: " <>
      show subExps <> ", " <> show eitherResp
  where
    -- The order of MeasurementRecs is not guaranteed to match the order of Measurements,
    -- so we have to match them somehow.
    -- Comparing Doubles by equality is a bad idea because conversion to SQL and back
    -- may lose precision, so we don't do it. At the same time, this conversion is hopefully
    -- monotonic, so if we sort both lists by keys the order should match.
    -- Note that, in theory, there can be multiple points with equal @(concentration, signal)@ pair,
    -- so the order matches only with respect to this equality. It shouldn't be a problem
    -- because if two points are equal, either both of them should be outliers or none of them.
    --
    -- After sorting both lists and zipping them, for each MeasurementRec we know its index in the
    -- original list of measurement and can check whether it's present in the outliers list.
    autoRemovedIds :: [MeasurementRec] -> NonEmpty Word -> [MeasurementId]
    autoRemovedIds measurementRecs (HS.fromList . toList -> outliers) =
      let sortedMeasurements = sortWith
            (\(_, m) -> (EReader.mConcentration m, EReader.mSignal m)) $
            zip [0..] measurements
          sortedRecs = sortWith
            (\MeasurementRec {..} -> (mConcentration, mSignal)) measurementRecs
      in flip mapMaybe (zip sortedMeasurements sortedRecs) $
          \((idx, _), MeasurementRec {..}) ->
          fromSqlSerial mMeasurementId <$ guard (idx `HS.member` outliers)
