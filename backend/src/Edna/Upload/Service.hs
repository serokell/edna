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

import Database.Beam.Query (all_, guard_, select, val_, (==.))
import Fmt (Buildable(..), pretty)
import Lens.Micro.Platform (at, (?~))

import Edna.DB.Integration (runSelectReturningOne')
import Edna.DB.Schema (CompoundT(..), EdnaSchema(..), TargetT(..), ednaSchema)
import Edna.ExperimentReader.Parser (parseExperimentXls)
import Edna.ExperimentReader.Types
import Edna.Setup
import Edna.Web.Types hiding (cName, tName)

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

compoundNameToId :: Text -> Edna (Maybe (SqlId Compound))
compoundNameToId compoundName =
  fmap (fmap mkSqlId) . runSelectReturningOne' $ select $ do
    compound <- all_ (esCompound ednaSchema)
    guard_ (cName compound ==. val_ compoundName)
    return $ cCompoundId compound

targetNameToId :: Text -> Edna (Maybe (SqlId Target))
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
      target <- maybeToLeft targetName <$> targetNameToId targetName
      compounds <- forM (keys targetMeasurements) $ \compoundName ->
        maybeToLeft compoundName <$> compoundNameToId compoundName
      return $ acc & at targetName ?~ FileSummaryItem
        { fsiTarget = target
        , fsiCompounds = compounds
        }

data UploadError =
    UEUnknownProject (SqlId Project)
  | UEUnknownTestMethodology (SqlId TestMethodology)
  deriving stock (Show, Eq)

instance Buildable UploadError where
  build = \case
    UEUnknownProject i -> "Unknown project: " <> build i
    UEUnknownTestMethodology i -> "Unknown test methodology: " <> build i

instance Exception UploadError where
  displayException = pretty

-- | Parse an experiment data file and save it to DB.
uploadFile ::
  SqlId Project -> SqlId TestMethodology -> Text -> Text -> LByteString ->
  Edna FileSummary
uploadFile proj methodology description fileName content = do
  uploadFile' proj methodology description fileName =<<
    either throwM pure (parseExperimentXls content)

uploadFile' ::
  SqlId Project -> SqlId TestMethodology -> Text -> Text -> FileContents ->
  Edna FileSummary
uploadFile' _proj _methodology _description _fileName fc = do
  -- Stub implementation
  parseFile' fc
