-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Tests for the Upload service.

module Test.UploadSpec
  ( spec
  ) where

import Universum

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.List ((!!))
import Lens.Micro ((?~))
import RIO (runRIO)
import Servant.Util (fullContent, noSorting)
import Test.Hspec (Spec, anyErrorCall, beforeAllWith, describe, it, shouldBe, shouldThrow)

import qualified Edna.Dashboard.Service as Dashboard
import qualified Edna.Library.Service as Library
import qualified Edna.Upload.Service as Upload

import Edna.Analysis.FourPL (Params4PLReq(..), Params4PLResp(..), analyse4PL)
import Edna.Config.Definition (MdeHost, ecMdeHost)
import Edna.Dashboard.Web.Types
  (ExperimentResp(..), ExperimentsResp(..), MeasurementResp(..), SubExperimentResp(..))
import Edna.ExperimentReader.Types (FileContents(..), Measurement(..), measurementToPair)
import Edna.Library.Web.Types (CompoundResp(crMde, crName), ProjectReq(..), TargetResp(..))
import Edna.Setup (edConfig)
import Edna.Upload.Service (UploadError(..), parseFile', uploadFile')
import Edna.Upload.Web.Types (FileSummary(..), FileSummaryItem(..), NameAndId(..), sortFileSummary)
import Edna.Util (IdType(..), SqlId(..))
import Edna.Util.URI (parseURI)
import Edna.Web.Types (URI, WithId(..))

import Test.Orphans ()
import Test.SampleData
import Test.Setup (runTestEdna, runWithInit, specWithContext)

spec :: Spec
spec = specWithContext $ startWithInitial $ do
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
      targets <- Library.getTargets noSorting fullContent
      compounds <- Library.getCompounds noSorting fullContent
      experiments <- Dashboard.getExperiments Nothing Nothing Nothing
        noSorting fullContent
      liftIO $ do
        summary `shouldBe` sortFileSummary sampleFileSummary'
        summary2 `shouldBe` sortFileSummary sampleFileSummary2
        checkTargets targets
        checkCompounds compounds
        checkExperiments (wItem <$> erExperiments experiments)

    it "successfully adds experiment with auto-detected outlier" $ runTestEdna $ do
      projId <- wiId <$> Library.addProject (ProjectReq "autoOutlierFile" Nothing)
      void $ uploadFileTest projId (SqlId 1) autoOutlierFile
      ExperimentsResp {..} <- Dashboard.getExperiments (Just projId) Nothing Nothing
        noSorting fullContent
      -- @autoOutlierFile@ has only 1 experiment and we are adding it into a
      -- completely new project. So we expect one experiment in the result.
      let [WithId _ ExperimentResp {..}] = erExperiments
      -- There should be 2 sub-experiments because we should find 1 outlier.
      -- One of them should be equal to primary, the other one is secondary.
      let [secondarySubExpId] = filter (/= erPrimarySubExperiment) erSubExperiments
      primaryMeasurements <- wItem <<$>> Dashboard.getMeasurements erPrimarySubExperiment
      secondaryMeasurements <- wItem <<$>> Dashboard.getMeasurements secondarySubExpId
      secondarySubExperiment <- wItem <$> Dashboard.getSubExperiment secondarySubExpId
      [(_, Right Params4PLResp {..})] <- analyse4PL [Params4PLReq (SqlId 1) True $
        map measurementToPair autoOutlierMeasurements]
      let Just (autoOutliers, secondaryParams4PL) = plrspNewSubExp
      let onlyDisabled = filter (not . mrIsEnabled)
      liftIO $ do
        length erSubExperiments `shouldBe` 2
        serResult secondarySubExperiment `shouldBe` Right secondaryParams4PL
        onlyDisabled primaryMeasurements `shouldBe` []
        let secondaryDisabled = flip map (toList autoOutliers) $ \idx ->
              let Measurement {..} = autoOutlierMeasurements !! fromIntegral idx
              in MeasurementResp
                  { mrConcentration = mConcentration
                  , mrSignal = mSignal
                  , mrIsEnabled = False
                  }
        onlyDisabled secondaryMeasurements `shouldBe` secondaryDisabled

    it "rollbacks targets and compounds due processing error" $ \ctx -> do
      runWithInit ctx addInitialData
      runRIO ctx (uploadFile' (SqlId 1) (SqlId 1) "" (error "e") "" sampleFile)
        `shouldThrow` anyErrorCall

      runRIO ctx $ do
        Library.getTargets noSorting fullContent >>= shouldBeZeroLength
        Library.getCompounds noSorting fullContent >>= shouldBeZeroLength
        Dashboard.getExperiments Nothing Nothing Nothing noSorting fullContent
          >>= shouldBeZeroLength . erExperiments

    describe "mde link construction" $ do
      it "sets Nothing if mdeHost is not set" $ runTestEdna $ do
        (_, cId) <- Upload.insertCompound compoundName1
        compound <- Library.getCompound cId
        liftIO $ checkMdeLink Nothing compound

      it "sets Nothing if compoundName ending IS NOT number" $ \ctx -> do
        let ctx' = ctx & edConfig . ecMdeHost ?~ mdeHost
        (_, cId) <- runRIO ctx' $ Upload.insertCompound "cmp9a"
        compound <- runRIO ctx' $ Library.getCompound cId
        checkMdeLink Nothing compound

      it "sets link if compoundName ending IS number" $ \ctx -> do
        let ctx' = ctx & edConfig . ecMdeHost ?~ mdeHost
        (_, cId) <- runRIO ctx' $ Upload.insertCompound "cmp9"
        compound <- runRIO ctx' $ Library.getCompound cId
        checkMdeLink (parseURI $ mdeHost <> "/9") compound
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

    shouldBeZeroLength :: (MonadIO m, Container t) => t -> m ()
    shouldBeZeroLength l = liftIO $ length l `shouldBe` 0

    mdeHost :: MdeHost
    mdeHost = "https://mde.edna/list"

    checkMdeLink :: Maybe URI -> WithId 'CompoundId CompoundResp -> IO ()
    checkMdeLink link WithId {..} = crMde wItem `shouldBe` link

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
