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

import Fmt ((+|), (|+))
import Lens.Micro.Platform (at, (?~))

import qualified Edna.Library.DB.Query as LQ
import qualified Edna.Upload.DB.Query as UQ

import Edna.Analysis.FourPL (analyse4PL)
import Edna.DB.Integration (transact)
import Edna.ExperimentReader.Parser (parseExperimentXls)
import Edna.ExperimentReader.Types as EReader
import Edna.Library.DB.Query (getMethodologyById, getProjectById)
import Edna.Library.DB.Schema as LDB
import Edna.Logging (logDebug, logMessage)
import Edna.Setup
import Edna.Upload.Error (UploadError(..))
import Edna.Upload.Web.Types (FileSummary(..), FileSummaryItem(..), NameAndId(..), sortFileSummary)
import Edna.Util as U
  (CompoundId, ExperimentFileId, MethodologyId, ProjectId, SqlId(..), TargetId, fromSqlSerial,
  justOrThrow)

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

      expFileId <- UQ.insertExperimentFile projId methodId (fcMetadata fc)
        description fileName fileBytes

      forM_ (toPairs fileMeasurements) $
        \(targetName, TargetMeasurements targetMeasurements) ->
          forM_ (toPairs targetMeasurements) $ \(compoundName, measurements) -> do
            let
              getId :: HasCallStack => Text -> HashMap Text (SqlId a) -> SqlId a
              getId name =
                fromMaybe (error $ "unexpected name: " <> name) .
                view (at name)
            let targetId = getId targetName targetToId
            let compoundId = getId compoundName compoundToId
            insertExperiment expFileId compoundId targetId measurements

      measurementsToSummary fileMeasurements

insertTarget :: Text -> Edna (Text, TargetId)
insertTarget targetName = (targetName,) . fromSqlSerial . tTargetId <$> LQ.insertTarget targetName

insertCompound :: Text -> Edna (Text, CompoundId)
insertCompound compoundName =
  (compoundName,) . fromSqlSerial . cCompoundId <$> LQ.insertCompound compoundName

insertExperiment :: ExperimentFileId -> CompoundId -> TargetId -> [Measurement] -> Edna ()
insertExperiment experimentFileId compoundId targetId measurements = do
  expId <- UQ.insertExperiment experimentFileId compoundId targetId
  -- TODO [EDNA-71] Pass actual points.
  analysisRes <- liftIO $ analyse4PL []
  subExpId <- UQ.insertSubExperiment expId analysisRes
  UQ.insertPrimarySubExperiment expId subExpId
  measurementIds <- UQ.insertMeasurements expId measurements
  let removedIds = map fst . filter (EReader.mIsOutlier . snd) $ zip measurementIds measurements
  UQ.insertRemovedMeasurements subExpId removedIds
