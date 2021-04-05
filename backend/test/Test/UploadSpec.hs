-- | Tests for the Upload service.

module Test.UploadSpec
  ( spec
  ) where

import Universum

import qualified Data.ByteString.Lazy as BSL

import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow, xit)

import Edna.ExperimentReader.Types (FileContents(..))
import Edna.Upload.Service (UploadError(..), parseFile', uploadFile')
import Edna.Upload.Web.Types (FileSummary(..), FileSummaryItem(..), NameAndId(..), sortFileSummary)
import Edna.Util (SqlId(..))

import Test.SampleData
import Test.Setup (runWithInit, withContext)

spec :: Spec
spec = withContext $ do
  describe "parseFile'" $ do
    it "returns empty summary for empty contents" $ \ctx -> do
      summary <- runWithInit ctx $ parseFile' (FileContents mempty sampleMetadata)
      summary `shouldBe` FileSummary []
    it "returns correct summary for sample data" $ \ctx -> do
      summary <- runWithInit ctx $ parseFile' sampleFile
      sortFileSummary summary `shouldBe` sortFileSummary sampleFileSummary
    -- TODO Need to be able to add projects and methodologies.
    xit "returns IDs when data is known" $ \ctx -> do
      summary <- runWithInit ctx $ uploadSampleFile >> parseFile' sampleFile
      sortFileSummary summary `shouldBe` sortFileSummary sampleFileSummary2
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
  [ FileSummaryItem (newNAI targetName1)
      [newNAI compoundName1, newNAI compoundName2, newNAI compoundName3]
  , FileSummaryItem (newNAI targetName2) [newNAI compoundName2]
  , FileSummaryItem (newNAI targetName3)
      [newNAI compoundName1, newNAI compoundName4]
  ]

oldNAI :: Word32 -> Text -> NameAndId anything
oldNAI i name = NameAndId name (Just (SqlId i))

sampleFileSummary2 :: FileSummary
sampleFileSummary2 = FileSummary
  [ FileSummaryItem (oldNAI 1 targetName1)
      [oldNAI 1 compoundName1, oldNAI 2 compoundName2, oldNAI 3 compoundName3]
  , FileSummaryItem (oldNAI 2 targetName2) [oldNAI 2 compoundName2]
  , FileSummaryItem (oldNAI 3 targetName3)
      [oldNAI 1 compoundName1, oldNAI 4 compoundName4]
  ]
