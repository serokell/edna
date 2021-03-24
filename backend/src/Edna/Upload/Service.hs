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
import qualified Database.Beam.Postgres.Full as Pg

import Database.Beam.Backend.SQL.BeamExtensions (unSerial)
import Database.Beam.Postgres (PgJSON(..), Postgres)
import Database.Beam.Query
  (QExpr, all_, default_, guard_, insert, insertExpressions, insertValues, lookup_, select, val_,
  (==.))
import Lens.Micro.Platform (at, (?~))

import Edna.DB.Integration
  (runInsert', runInsertReturningList', runInsertReturningOne', runSelectReturningOne', transact)
import Edna.DB.Schema
import Edna.ExperimentReader.Parser (parseExperimentXls)
import Edna.ExperimentReader.Types as EReader
import Edna.Library.DB.Schema as LDB
import Edna.Setup
import Edna.Upload.Error (UploadError(..))
import Edna.Util as U (IdType(..), SqlId(..))
import Edna.Web.Types

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

compoundNameToId :: Text -> Edna (Maybe (SqlId 'U.CompoundId))
compoundNameToId compoundName =
  fmap (fmap mkSqlId) . runSelectReturningOne' $ select $ do
    compound <- all_ (esCompound ednaSchema)
    guard_ (cName compound ==. val_ compoundName)
    return $ cCompoundId compound

targetNameToId :: Text -> Edna (Maybe (SqlId 'U.TargetId))
targetNameToId targetName =
  fmap (fmap mkSqlId) . runSelectReturningOne' $ select $ do
    target <- all_ (esTarget ednaSchema)
    guard_ (tName target ==. val_ targetName)
    return $ tTargetId target

mkSqlId :: Integral x => x -> SqlId y
mkSqlId = SqlId . fromIntegral

measurementsToSummary :: HashMap Text TargetMeasurements -> Edna FileSummary
measurementsToSummary =
  fmap (FileSummary . toList) . foldM step mempty . toPairs
  where
    step ::
      HashMap Text FileSummaryItem -> (Text, TargetMeasurements) ->
      Edna $ HashMap Text FileSummaryItem
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
  SqlId 'U.ProjectId -> SqlId 'MethodologyId -> Text -> Text -> LByteString ->
  Edna FileSummary
uploadFile proj methodology description fileName content = do
  uploadFile' proj methodology description fileName content =<<
    either throwM pure (parseExperimentXls content)

uploadFile' ::
  SqlId 'U.ProjectId -> SqlId 'MethodologyId -> Text -> Text -> LByteString ->
  FileContents -> Edna FileSummary
uploadFile' projSqlId@(SqlId proj) methodSqlId@(SqlId method)
  description fileName fileBytes fc = do
    let projId = LDB.ProjectId $ fromIntegral proj
    let methodId = TestMethodologyId $ fromIntegral method
    runSelectReturningOne' (lookup_ (esProject ednaSchema) projId)
      `whenNothingM_`
      throwM (UEUnknownProject projSqlId)
    runSelectReturningOne' (lookup_ (esTestMethodology ednaSchema) methodId)
      `whenNothingM_`
      throwM (UEUnknownTestMethodology methodSqlId)

    transact $ insertAll proj method
  where
    insertAll :: HasCallStack => Word32 -> Word32 -> Edna FileSummary
    insertAll projId methodId = do
      let fileMeasurements = fcMeasurements fc
      targetToId <- HM.fromList <$> mapM insertTarget (keys fileMeasurements)
      let compounds = hashNub $ flip concatMap (elems fileMeasurements) $
            \(TargetMeasurements targetMeasurements) -> keys targetMeasurements
      compoundToId <- HM.fromList <$> mapM insertCompound compounds

      expFileId <- insertExperimentFile projId methodId (fcMetadata fc)
        description fileName fileBytes

      forM_ (toPairs fileMeasurements) $
        \(targetName, TargetMeasurements targetMeasurements) ->
          forM_ (toPairs targetMeasurements) $ \(compoundName, measurements) -> do
            let
              getId :: HasCallStack => Text -> HashMap Text Word32 -> Word32
              getId name =
                fromMaybe (error $ "unexpected name: " <> name) .
                view (at name)
            let targetId = getId targetName targetToId
            let compoundId = getId compoundName compoundToId
            insertExperiment expFileId compoundId targetId measurements

      measurementsToSummary fileMeasurements

insertTarget :: Text -> Edna (Text, Word32)
insertTarget targetName = (targetName,) . unSqlId <$> do
  runInsert' $ Pg.insert
    (esTarget ednaSchema)
    (insertExpressions [TargetRec default_ (val_ targetName) default_])
    (Pg.onConflict (Pg.conflictingFields tName) Pg.onConflictDoNothing)
  fromMaybe (error $ "added target not found: " <> targetName)  <$>
    targetNameToId targetName

insertCompound :: Text -> Edna (Text, Word32)
insertCompound compoundName = (compoundName,) . unSqlId <$> do
  runInsert' $ Pg.insert
    (esCompound ednaSchema)
    (insertExpressions [CompoundRec default_ (val_ compoundName) default_ default_])
    (Pg.onConflict (Pg.conflictingFields cName) Pg.onConflictDoNothing)
  fromMaybe (error $ "added compound not found: " <> compoundName)  <$>
    compoundNameToId compoundName

insertExperimentFile ::
  Word32 -> Word32 -> FileMetadata -> Text -> Text -> LByteString -> Edna Word32
insertExperimentFile projId methodId meta descr fileName blob = do
  unSerial . efExperimentFileId <$> runInsertReturningOne'
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

insertExperiment :: Word32 -> Word32 -> Word32 -> [Measurement] -> Edna ()
insertExperiment experimentFileId compoundId targetId measurements = do
  expId <-
    unSerial . eExperimentId <$> runInsertReturningOne'
    (insert (esExperiment ednaSchema) $ insertExpressions [experimentRec])
  subExpId <-
    unSerial . seSubExperimentId <$> runInsertReturningOne'
    (insert (esSubExperiment ednaSchema) $ insertExpressions [subExperimentRec expId])
  let
    measurementRecs :: [MeasurementT (QExpr Postgres s)]
    measurementRecs = flip map measurements $ \measurement -> MeasurementRec
      { mMeasurementId = default_
      , mExperimentId = val_ expId
      , mConcentration = val_ (EReader.mConcentration measurement)
      , mSignal = val_ (EReader.mSignal measurement)
      , mIsOutlier = val_ (EReader.mIsOutlier measurement)
      }
  measurementIds <- map (unSerial . mMeasurementId) <$> runInsertReturningList'
    (insert (esMeasurement ednaSchema) $ insertExpressions measurementRecs)
  let removedIds =
        map fst . filter (EReader.mIsOutlier . snd) $
        zip measurementIds measurements
  let removedMeasurementsRecs = flip map removedIds $ \removedId ->
        RemovedMeasurementsRec subExpId removedId
  runInsert' $ insert (esRemovedMeasurements ednaSchema) $ insertValues
    removedMeasurementsRecs
  where
    experimentRec :: ExperimentT (QExpr Postgres s)
    experimentRec = ExperimentRec
      { eExperimentId = default_
      , eExperimentFileId = val_ experimentFileId
      , eCompoundId = val_ compoundId
      , eTargetId = val_ targetId
      }

    subExperimentRec :: Word32 -> SubExperimentT (QExpr Postgres s)
    subExperimentRec experimentId = SubExperimentRec
      { seSubExperimentId = default_
      , seAnalysisMethodId = val_ theOnlyAnalysisMethodId
      , seExperimentId = val_ experimentId
      , seIsSuspicious = val_ False
      , seResult = val_ (PgJSON 10)  -- stub value, will be computed later
      }
