-- | Tests for the Upload service.

module Test.UploadSpec
  ( spec
  ) where

import Universum

import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM

import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow, xit)

import Edna.ExperimentReader.Types
  (FileContents(..), FileMetadata(..), Measurement(..), TargetMeasurements(..))
import Edna.Upload.Service (UploadError(..), parseFile', uploadFile')
import Edna.Web.Types (FileSummary(..), FileSummaryItem(..), SqlId(..))

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

sampleFile :: FileContents
sampleFile = FileContents {..}
  where
    fcMeasurements = HM.fromList . map (second TargetMeasurements) $
      [ ("tar1", targetMeasurements1)
      , ("tar2", targetMeasurements2)
      , ("tar3", targetMeasurements3)
      ]
    fcMetadata = sampleMetadata

targetMeasurements1 :: HashMap Text [Measurement]
targetMeasurements1 = HM.fromList
  [ ("comp1", [m1, m2, m3])
  , ("comp2", [m2, m4, m5])
  , ("comp3", [m1, m3, m4])
  ]

targetMeasurements2 :: HashMap Text [Measurement]
targetMeasurements2 = HM.fromList
  [ ("comp2", [m1, m2, m5])
  ]

targetMeasurements3 :: HashMap Text [Measurement]
targetMeasurements3 = HM.fromList
  [ ("comp1", [m1, m5])
  , ("comp4", [m3, m4, m5])
  ]

m1, m2, m3, m4, m5 :: Measurement
m1 = Measurement
  { mConcentration = 1.5
  , mSignal = 200
  , mIsOutlier = False
  }
m2 = m1 {mConcentration = 3, mSignal = 400}
m3 = m1 {mConcentration = 6, mSignal = 600}
m4 = m1 {mConcentration = 12, mSignal = 800}
m5 = Measurement
  { mConcentration = 6
  , mSignal = 5000
  , mIsOutlier = True
  }

sampleFileSummary :: FileSummary
sampleFileSummary = FileSummary
  [ FileSummaryItem (Right "tar1") [Right "comp3", Right "comp2", Right "comp1"]
  , FileSummaryItem (Right "tar2") [Right "comp2"]
  , FileSummaryItem (Right "tar3") [Right "comp4", Right "comp1"]
  ]

sampleFileSummary2 :: FileSummary
sampleFileSummary2 = FileSummary
  [ FileSummaryItem (Left (SqlId 1))
      [Left (SqlId 3), Left (SqlId 2), Left (SqlId 1)]
  , FileSummaryItem (Left (SqlId 2)) [Left (SqlId 2)]
  , FileSummaryItem (Left (SqlId 3)) [Left (SqlId 4), Left (SqlId 1)]
  ]

sampleMetadata :: FileMetadata
sampleMetadata = FileMetadata ["foo", "room", "mood"]

sortSummary :: FileSummary -> FileSummary
sortSummary (FileSummary lst) = FileSummary (sort lst)
