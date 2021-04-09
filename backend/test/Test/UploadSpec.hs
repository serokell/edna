-- | Tests for the Upload service.

module Test.UploadSpec
  ( spec
  ) where

import Universum

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import RIO (runRIO)
import Test.Hspec (Spec, beforeAllWith, describe, it, shouldBe, shouldThrow)

import qualified Edna.Dashboard.Service as Dashboard
import qualified Edna.Library.Service as Library

import Edna.Dashboard.Web.Types (ExperimentResp(..), ExperimentsResp(..))
import Edna.ExperimentReader.Types (FileContents(..))
import Edna.Library.Web.Types (CompoundResp(crName), TargetResp(..))
import Edna.Upload.Service (UploadError(..), parseFile', uploadFile')
import Edna.Upload.Web.Types (FileSummary(..), FileSummaryItem(..), NameAndId(..), sortFileSummary)
import Edna.Util (IdType(..), SqlId(..))
import Edna.Web.Types (WithId(..))

import Test.SampleData
import Test.Setup (runTestEdna, runWithInit, withContext)

spec :: Spec
spec = withContext $ startWithInitial $ do
  describe "parseFile'" $ do
    it "returns empty summary for empty contents" $ runTestEdna $ do
      summary <- parseFile' (FileContents mempty sampleMetadata)
      liftIO $ summary `shouldBe` FileSummary []
    it "returns correct summary for sample data" $ runTestEdna $ do
      summary <- parseFile' sampleFile
      liftIO $
        summary `shouldBe` sortFileSummary sampleFileSummary
    it "returns IDs when data is known" $ runTestEdna $ do
      summary <-
        uploadFileTest (SqlId 1) (SqlId 1) sampleFile >> parseFile' sampleFile
      liftIO $
        summary `shouldBe` sortFileSummary sampleFileSummary'
  -- Starting with initial state here because there is one successful upload above.
  describe "uploadFile'" $ startWithInitial $ do
    it "fails when referenced project does not exist" $ \ctx -> do
      runRIO ctx (uploadFileTest unknownSqlId (SqlId 1) sampleFile) `shouldThrow`
        (== UEUnknownProject unknownSqlId)
    it "fails when referenced test methodology does not exist" $ \ctx -> do
      runRIO ctx (uploadFileTest (SqlId 1) unknownSqlId sampleFile) `shouldThrow`
        (== UEUnknownTestMethodology unknownSqlId)
    -- Note that there are some checks in LibrarySpec and DashboardSpec as well.
    it "successfully adds targets and compounds" $ runTestEdna $ do
      summary <- uploadFile' (SqlId 1) (SqlId 1) "file description" "file name"
        "blob" sampleFile
      summary2 <- uploadFile' (SqlId 2) (SqlId 2) "file description 2" "file name 2"
        "blob2" sampleFile2
      targets <- Library.getTargets Nothing Nothing Nothing
      compounds <- Library.getCompounds Nothing Nothing Nothing
      experiments <- Dashboard.getExperiments Nothing Nothing Nothing
      liftIO $ do
        summary `shouldBe` sortFileSummary sampleFileSummary'
        summary2 `shouldBe` sortFileSummary sampleFileSummary2
        checkTargets targets
        checkCompounds compounds
        checkExperiments (wItem <$> erExperiments experiments)
  where
    addInitialData = addSampleProjects >> addSampleMethodologies
    startWithInitial =
      beforeAllWith (\ctx -> ctx <$ runWithInit ctx addInitialData)

    checkTargets :: [WithId 'TargetId TargetResp] -> IO ()
    checkTargets responses =
      Map.fromList (map (\WithId {..} -> (trName wItem, trProjects wItem)) responses) `shouldBe`
      expectedTargets

    expectedTargets = Map.fromList
      [ (targetName1, [projectName1, projectName2 ])
      , (targetName2, [projectName1])
      , (targetName3, [projectName1])
      , (targetName4, [projectName2])
      ]

    checkCompounds :: [WithId 'CompoundId CompoundResp] -> IO ()
    checkCompounds responses =
      Set.fromList (map (\WithId {..} -> crName wItem) responses) `shouldBe`
      Set.fromList [compoundName1, compoundName2, compoundName3, compoundName4, compoundName5]

    checkExperiments :: [ExperimentResp] -> IO ()
    checkExperiments responses = do
      length (filter (\ExperimentResp {..} -> erProject == SqlId 1) responses)
        `shouldBe` 6
      length (filter (\ExperimentResp {..} -> erProject == SqlId 2) responses)
        `shouldBe` 3
      forM_ responses $ \ExperimentResp {..} ->
        length erSubExperiments `shouldBe` 1

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

sampleFileSummary' :: FileSummary
sampleFileSummary' = FileSummary
  [ FileSummaryItem (oldNAI 1 targetName1)
      [oldNAI 1 compoundName1, oldNAI 2 compoundName2, oldNAI 3 compoundName3]
  , FileSummaryItem (oldNAI 2 targetName2) [oldNAI 2 compoundName2]
  , FileSummaryItem (oldNAI 3 targetName3)
      [oldNAI 1 compoundName1, oldNAI 4 compoundName4]
  ]

sampleFileSummary2 :: FileSummary
sampleFileSummary2 = FileSummary
  [ FileSummaryItem (oldNAI 1 targetName1) [oldNAI 2 compoundName2]
  , FileSummaryItem (oldNAI 5 targetName4)
      [oldNAI 1 compoundName1, oldNAI 7 compoundName5]
  ]
