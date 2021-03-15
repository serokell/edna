-- | Implementation of file upload functionality

module Edna.Upload.Service
  ( parseFile
  , uploadFile
  ) where

import Universum

import Lens.Micro.Platform (at)

import Edna.ExperimentReader.Parser (parseExperimentXls)
import Edna.Setup
import Edna.Web.Types

-- | Parse contents of an experiment data file and return as 'FileSummary'.
-- Uses database to determine which targets are new.
parseFile :: LByteString -> Edna FileSummary
parseFile content =
  measurementsToSummary =<< either throwM pure (parseExperimentXls content)

-- Need DB access for that
compoundNameToId :: Text -> Edna (Maybe (SqlId Compound))
compoundNameToId _ = pure Nothing

targetNameToId :: Text -> Edna (Maybe (SqlId Target))
targetNameToId _ = pure Nothing

-- TODO: once we drop legacy API, I think we should drop 'ExperimentalMeasurement'
-- data type completely and update the parser to return something more compact.
measurementsToSummary :: [ExperimentalMeasurement] -> Edna FileSummary
measurementsToSummary =
  fmap (FileSummary . toList) . foldM step mempty
  where
    step ::
      Map Text FileSummaryItem -> ExperimentalMeasurement ->
      Edna $ Map Text FileSummaryItem
    step acc ExperimentalMeasurement {..} = do
      compound <- maybeToLeft emCompoundId <$> compoundNameToId emCompoundId
      target <- maybeToLeft emCompoundId <$> targetNameToId emTargetId
      let
        mapExisting :: Maybe FileSummaryItem -> Maybe FileSummaryItem
        mapExisting = \case
          Nothing -> Just $ FileSummaryItem target [compound]
          Just fsi -> Just $ fsi
            { fsiCompounds = compound : fsiCompounds fsi
            }
      return $ acc & at emTargetId %~ mapExisting

-- | Parse an experiment data file and save it to DB.
uploadFile ::
  SqlId Project -> SqlId TestMethodology -> Text -> Text -> LByteString ->
  Edna FileSummary
uploadFile _proj _methodology _description _fileName content = do
  _parsedFile <- either throwM pure (parseExperimentXls content)
  -- Stub implementation
  parseFile content
