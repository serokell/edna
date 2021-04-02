-- | Tests for the Upload service.

module Test.UploadSpec
  ( spec
  ) where

import Universum

import qualified Data.ByteString.Lazy as BSL

import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow, xit)

import Edna.ExperimentReader.Types (FileContents(..))
import Edna.Upload.Service (UploadError(..), parseFile', uploadFile')
import Edna.Upload.Web.Types (FileSummary(..), FileSummaryItem(..), NameAndId(..))
import Edna.Util (SqlId(..))

import Test.SampleData (sampleFile, sampleMetadata)
import Test.Setup (runWithInit, withContext)

spec :: Spec
spec = withContext $ do
  describe "parseFile'" $ do
    it "returns empty summary for empty contents" $ \ctx -> do
      summary <- runWithInit ctx $ parseFile' (FileContents mempty sampleMetadata)
      summary `shouldBe` FileSummary []
    it "returns correct summary for sample data" $ \ctx -> do
      summary <- runWithInit ctx $ parseFile' sampleFile
      sortSummary summary `shouldBe` sortSummary sampleFileSummary
    -- TODO Need to be able to add projects and methodologies.
    xit "returns IDs when data is known" $ \ctx -> do
      summary <- runWithInit ctx $ uploadSampleFile >> parseFile' sampleFile
      sortSummary summary `shouldBe` sortSummary sampleFileSummary2
  describe "uploadFile'" $ do
    it "fails when referenced project does not exist" $ \ctx -> do
      runWithInit ctx uploadSampleFile `shouldThrow`
        (== UEUnknownProject (SqlId 1))
    xit "fails when referenced test methodology does not exist" $ \ctx -> do
      runWithInit ctx uploadSampleFile `shouldThrow`
        (== UEUnknownTestMethodology (SqlId 1))
    xit "TODO: positive tests" $ \ctx ->
      runWithInit ctx pass
  where
    uploadSampleFile =
      uploadFile' (SqlId 1) (SqlId 1) "descr" "name" BSL.empty sampleFile

newNAI :: Text -> NameAndId anything
newNAI name = NameAndId name Nothing

sampleFileSummary :: FileSummary
sampleFileSummary = FileSummary
  [ FileSummaryItem (newNAI "tar1") [newNAI "comp3", newNAI "comp2", newNAI "comp1"]
  , FileSummaryItem (newNAI "tar2") [newNAI "comp2"]
  , FileSummaryItem (newNAI "tar3") [newNAI "comp4", newNAI "comp1"]
  ]

oldNAI :: Word32 -> Text -> NameAndId anything
oldNAI i name = NameAndId name (Just (SqlId i))

sampleFileSummary2 :: FileSummary
sampleFileSummary2 = FileSummary
  [ FileSummaryItem (oldNAI 1 "tar1")
      [oldNAI 3 "tar3", oldNAI 2 "tar2", oldNAI 1 "tar1"]
  , FileSummaryItem (oldNAI 2 "tar2") [oldNAI 2 "comp2"]
  , FileSummaryItem (oldNAI 3 "tar3") [oldNAI 4 "tar4", oldNAI 1 "tar1"]
  ]

sortSummary :: FileSummary -> FileSummary
sortSummary (FileSummary lst) = FileSummary (sort lst)
