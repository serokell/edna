-- | Tests for the Dashboard service.

module Test.DashboardSpec
  ( spec
  ) where

import Universum

import RIO (runRIO)
import Test.Hspec (Spec, beforeAllWith, describe, it, shouldBe, shouldSatisfy, shouldThrow)

import Edna.Dashboard.Error (DashboardError(..))
import Edna.Dashboard.Service
  (deleteSubExperiment, getExperiments, getMeasurements, getSubExperiment,
  setIsSuspiciousSubExperiment, setNameSubExperiment)
import Edna.Dashboard.Web.Types
  (ExperimentResp(..), ExperimentsResp(..), MeasurementResp(..), SubExperimentResp(..))
import Edna.ExperimentReader.Types (Measurement(..))
import Edna.Util (SqlId(..), SubExperimentId)
import Edna.Web.Types (WithId(..))

import Test.Orphans ()
import Test.SampleData
import Test.Setup (runTestEdna, runWithInit, withContext)

-- TODO [EDNA-73] Add the following tests:
-- 1. makePrimarySubExperiment
-- 2. Successful deleteSubExperiment
-- 3. Addition of sub-experiments (functionality will be implemented in EDNA-73)
spec :: Spec
spec = withContext $ do
  beforeAllWith (\ctx -> ctx <$ runWithInit ctx addSampleData) $ do
    describe "getters" $ do
      describe "getExperiments" $ do
        it "returns all experiments and no IC50 with no filters" $ runTestEdna $ do
          ExperimentsResp {..} <- getExperiments Nothing Nothing Nothing
          liftIO $ do
            length erExperiments `shouldBe` 12
            erMeanIC50 `shouldBe` Nothing
        it "filters by project correctly" $ runTestEdna $ do
          ExperimentsResp {..} <- getExperiments (Just $ SqlId 1) Nothing Nothing
          liftIO $ do
            length erExperiments `shouldBe` 6
            erMeanIC50 `shouldBe` Nothing
            forM_ erExperiments $ \(WithId _ ExperimentResp {..}) ->
              erProject `shouldBe` SqlId 1
        it "filters by project and compound correctly" $ runTestEdna $ do
          let compoundId = SqlId 2
          ExperimentsResp {..} <-
            getExperiments (Just $ SqlId 1) (Just compoundId) Nothing
          liftIO $ do
            length erExperiments `shouldBe` 2
            erMeanIC50 `shouldBe` Nothing
            forM_ erExperiments $ \(WithId _ ExperimentResp {..}) ->
              erCompound `shouldBe` compoundId
        it "filters by 3 filters correctly and returns mean IC50" $ runTestEdna $ do
          let compoundId = SqlId 2
          let targetId = SqlId 2
          ExperimentsResp {..} <-
            getExperiments (Just $ SqlId 1) (Just compoundId) (Just targetId)
          liftIO $ do
            length erExperiments `shouldBe` 1
            erMeanIC50 `shouldBe` Just 3
            forM_ erExperiments $ \(WithId _ ExperimentResp {..}) ->
              erTarget `shouldBe` targetId
      describe "getSubExperiment" $ do
        it "returns correct results for sub-experiments 1-6" $ runTestEdna $ do
          resps <- forM validSubExperimentIds $ \subExpId -> do
            WithId gotId gotResp <- getSubExperiment subExpId
            liftIO $ gotId `shouldBe` subExpId
            return gotResp
          -- Very simple check
          liftIO $ forM_ resps $ \resp ->
            resp `shouldSatisfy` ((False == ) . serIsSuspicious)
        it "fails for unknown sub-experiment" $ \ctx -> do
          runRIO ctx (getSubExperiment unknownSubExpId) `shouldThrow`
            (== DESubExperimentNotFound unknownSubExpId)
      describe "getMeasurements" $ do
        it "returns correct measurements for sub-experiments 1-6" $ runTestEdna $ do
          [ measurements1
            , measurements2
            , measurements3
            , measurements4
            , measurements5
            , measurements6
            ] <- mapM getMeasurements validSubExperimentIds
          let
            toMeasurementResp :: Measurement -> MeasurementResp
            toMeasurementResp Measurement {..} = MeasurementResp
              { mrConcentration = mConcentration
              , mrSignal = mSignal
              , mrIsEnabled = not mIsOutlier
              }
          -- These lists match lists in @targetMeasurementsX@ values from SampleData.
          liftIO $ do
            measurements1 `shouldBe` map toMeasurementResp [m1, m2, m3]
            measurements2 `shouldBe` map toMeasurementResp [m2, m4, m5]
            measurements3 `shouldBe` map toMeasurementResp [m1, m3, m4]
            measurements4 `shouldBe` map toMeasurementResp [m1, m2, m5]
            measurements5 `shouldBe` map toMeasurementResp [m1, m5]
            measurements6 `shouldBe` map toMeasurementResp [m3, m4, m5]
        it "returns empty list for unknown sub-experiment" $ runTestEdna $ do
          measurements <- getMeasurements unknownSubExpId
          liftIO $ measurements `shouldBe` []
    describe "setNameSubExperiment" $ do
      it "updates the name of a sub-experiment to the given one" $ runTestEdna do
        resp <- setNameSubExperiment sampleSubExpId "newName"
        getResp <- getSubExperiment sampleSubExpId
        liftIO $ do
          resp `shouldBe` getResp
          serName (wItem resp) `shouldBe` "newName"
    describe "setIsSuspiciousSubExperiment" $ do
      it "updates 'isSuspicious' flag of a sub-experiment" $ runTestEdna $ do
        resp <- setIsSuspiciousSubExperiment sampleSubExpId True
        getResp <- getSubExperiment sampleSubExpId
        liftIO $ do
          resp `shouldBe` getResp
          serIsSuspicious (wItem resp) `shouldBe` True
    describe "deleteSubExperiment" $ do
      it "can't delete primary sub-experiment" $ \ctx -> do
        runRIO ctx (deleteSubExperiment sampleSubExpId) `shouldThrow`
          (== DECantDeletePrimary sampleSubExpId)
  where
    addSampleData = do
      addSampleProjects
      addSampleMethodologies
      uploadFileTest (SqlId 2) (SqlId 1) sampleFile
      uploadFileTest (SqlId 1) (SqlId 1) sampleFile

    sampleSubExpId, unknownSubExpId :: SubExperimentId
    sampleSubExpId = SqlId 1
    unknownSubExpId = SqlId 100

    validSubExperimentIds :: [SubExperimentId]
    validSubExperimentIds = map SqlId [1 .. 6]
