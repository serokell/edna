{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Tests for the Dashboard service.

module Test.DashboardSpec
  ( spec
  ) where

import Universum

import qualified Data.ByteString.Lazy as BSL

import RIO (runRIO)
import Test.Hspec (Spec, beforeAllWith, describe, it, shouldBe, shouldSatisfy, shouldThrow)

import qualified Edna.Library.Service as Library
import qualified Edna.Upload.Service as Upload

import Edna.Dashboard.Error (DashboardError(..))
import Edna.Dashboard.Service
  (deleteSubExperiment, getExperiments, getMeasurements, getSubExperiment,
  setIsSuspiciousSubExperiment, setNameSubExperiment)
import Edna.Dashboard.Web.Types
  (ExperimentResp(..), ExperimentsResp(..), MeasurementResp(..), SubExperimentResp(..))
import Edna.ExperimentReader.Types (Measurement(..))
import Edna.Library.Web.Types (MethodologyReqResp(..), ProjectReq(..))
import Edna.Setup (Edna, EdnaContext)
import Edna.Util (SqlId(..), SubExperimentId)
import Edna.Web.Types (WithId(..))

import Test.Orphans ()
import Test.SampleData
import Test.Setup (runWithInit, withContext)

-- TODO [EDNA-73] Add the following tests:
-- 1. makePrimarySubExperiment
-- 2. Successful deleteSubExperiment
-- 3. Addition of sub-experiments (functionality will be implemented in EDNA-73)
spec :: Spec
spec = withContext $ do
  beforeAllWith (\ctx -> ctx <$ runWithInit ctx addSampleData) $ do
    describe "getters" $ do
      describe "getExperiments" $ do
        it "returns all experiments and no IC50 with no filters" $ runTest $ do
          ExperimentsResp {..} <- getExperiments Nothing Nothing Nothing
          liftIO $ do
            length erExperiments `shouldBe` 12
            erMeanIC50 `shouldBe` Nothing
        it "filters by project correctly" $ runTest $ do
          ExperimentsResp {..} <- getExperiments (Just $ SqlId 1) Nothing Nothing
          liftIO $ do
            length erExperiments `shouldBe` 6
            erMeanIC50 `shouldBe` Nothing
            forM_ erExperiments $ \(WithId _ ExperimentResp {..}) ->
              erProject `shouldBe` SqlId 1
        it "filters by project and compound correctly" $ runTest $ do
          ExperimentsResp {..} <-
            getExperiments (Just $ SqlId 1) (Just $ SqlId 3) Nothing
          liftIO $ do
            length erExperiments `shouldBe` 2
            erMeanIC50 `shouldBe` Nothing
            forM_ erExperiments $ \(WithId _ ExperimentResp {..}) ->
              erCompound `shouldBe` SqlId 3
        it "filters by 3 filters correctly and returns mean IC50" $ runTest $ do
          ExperimentsResp {..} <-
            getExperiments (Just $ SqlId 1) (Just $ SqlId 3) (Just $ SqlId 2)
          liftIO $ do
            length erExperiments `shouldBe` 1
            erMeanIC50 `shouldBe` Just 3
            forM_ erExperiments $ \(WithId _ ExperimentResp {..}) ->
              erTarget `shouldBe` SqlId 2
      describe "getSubExperiment" $ do
        it "returns correct results for sub-experiments 1-6" $ runTest $ do
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
        it "returns correct measurements for sub-experiments 1-6" $ runTest $ do
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
          -- The order in which they are added depends on hashes (since we are using hashmaps).
          liftIO $ do
            measurements1 `shouldBe` map toMeasurementResp [m3, m4, m5]
            measurements2 `shouldBe` map toMeasurementResp [m1, m5]
            measurements3 `shouldBe` map toMeasurementResp [m1, m2, m5]
            measurements4 `shouldBe` map toMeasurementResp [m1, m3, m4]
            measurements5 `shouldBe` map toMeasurementResp [m2, m4, m5]
            measurements6 `shouldBe` map toMeasurementResp [m1, m2, m3]
        it "returns empty list for unknown sub-experiment" $ runTest $ do
          measurements <- getMeasurements unknownSubExpId
          liftIO $ measurements `shouldBe` []
    describe "setNameSubExperiment" $ do
      it "updates the name of a sub-experiment to the given one" $ runTest do
        resp <- setNameSubExperiment sampleSubExpId "newName"
        getResp <- getSubExperiment sampleSubExpId
        liftIO $ do
          resp `shouldBe` getResp
          serName (wItem resp) `shouldBe` "newName"
    describe "setIsSuspiciousSubExperiment" $ do
      it "updates 'isSuspicious' flag of a sub-experiment" $ runTest $ do
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
    addSampleData =
      Library.addProject (ProjectReq "project1" (Just "First project")) *>
      Library.addProject (ProjectReq "project2" (Just "Second project")) *>
      Library.addMethodology (MethodologyReqResp "methodology1" Nothing Nothing) *>
      Upload.uploadFile' (SqlId 2) (SqlId 1) "descr" "name" BSL.empty sampleFile *>
      Upload.uploadFile' (SqlId 1) (SqlId 1) "descr" "name" BSL.empty sampleFile $>
      ()

    sampleSubExpId, unknownSubExpId :: SubExperimentId
    sampleSubExpId = SqlId 1
    unknownSubExpId = SqlId 100

    validSubExperimentIds :: [SubExperimentId]
    validSubExperimentIds = map SqlId [1 .. 6]

    runTest :: Edna () -> EdnaContext -> IO ()
    runTest = flip runRIO
