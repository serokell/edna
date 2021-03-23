-- | Implementation of file upload functionality

module Edna.Upload.Service
  ( parseFile
  , uploadFile

  -- * Exported for tests
  , parseFile'
  ) where

import Universum

import Lens.Micro.Platform (at, (?~))

import Edna.ExperimentReader.Parser (parseExperimentXls)
import Edna.ExperimentReader.Types
import Edna.Setup
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

-- Need DB access for that
compoundNameToId :: Text -> Edna (Maybe (SqlId Compound))
compoundNameToId _ = pure Nothing

targetNameToId :: Text -> Edna (Maybe (SqlId Target))
targetNameToId _ = pure Nothing

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


-- | Parse an experiment data file and save it to DB.
uploadFile ::
  SqlId Project -> SqlId TestMethodology -> Text -> Text -> LByteString ->
  Edna FileSummary
uploadFile _proj _methodology _description _fileName content = do
  _parsedFile <- either throwM pure (parseExperimentXls content)
  -- Stub implementation
  parseFile content
