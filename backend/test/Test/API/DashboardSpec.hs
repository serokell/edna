-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | API testing for Dashboard.

module Test.API.DashboardSpec
  ( spec
  ) where

import Universum

import Network.HTTP.Types (badRequest400, notFound404)
import Servant.API.ResponseHeaders (getResponse)
import Servant.Client (ClientEnv)
import Servant.Client.Core (RunClient)
import Servant.Client.Generic (AsClientT, genericClient)
import Servant.Util (fullContent, noSorting)
import Test.Hspec
  (Spec, SpecWith, beforeAllWith, describe, it, shouldBe, shouldSatisfy, shouldThrow)

import qualified Edna.Library.Service as Library

import Edna.Analysis.FourPL (AnalysisResult)
import Edna.Dashboard.Service (newSubExperiment)
import Edna.Dashboard.Web.API (DashboardEndpoints(..))
import Edna.Dashboard.Web.Types
  (ExperimentFileBlob(..), ExperimentMetadata(..), ExperimentResp(..), ExperimentsResp(..),
  MeasurementResp(..), NewSubExperimentReq(..), SubExperimentResp(..))
import Edna.ExperimentReader.Types (mConcentration, mSignal, unFileMetadata)
import Edna.Library.Web.Types (MethodologyReq(..))
import Edna.Util (ExperimentId, IdType(..), MeasurementId, SqlId(..), SubExperimentId)
import Edna.Web.Types (WithId(..))

import Test.API.Util (apiTry, errorWithStatus)
import Test.Orphans ()
import Test.SampleData
import Test.Setup (runWithInit, specWithContextAndEnv)

spec :: Spec
spec = specWithContextAndEnv $ do
  beforeAllWith (\(ctx, env) -> runWithInit ctx addSampleData $> env) $ do
    describe "experiment API" experimentSpec
    describe "subExperiment API" subExperimentSpec
  where
    addSampleData = do
      addSampleProjects
      addSampleMethodologies
      toDeleteId <- wiId <$> Library.addMethodology (MethodologyReq "toDelete" Nothing Nothing)
      uploadFileTest (SqlId 1) (SqlId 1) sampleFile
      uploadFileTest (SqlId 2) (SqlId 2) sampleFile
      uploadFileTest (SqlId 2) toDeleteId sampleFile
      void $ Library.deleteMethodology toDeleteId
      newSubExperiment (SqlId 1) NewSubExperimentReq
        { nserName = "qwe"
        , nserChanges = mempty
        }

type Experiment = WithId 'ExperimentId ExperimentResp

experimentSpec :: SpecWith ClientEnv
experimentSpec = do
  describe "GET /experiments" $ do
    it "allows to list all experiments" $ \env -> do
      ExperimentsResp {..} <- apiTry env $
        deGetExperiments dClient Nothing Nothing Nothing noSorting fullContent
      let experiments@(experiment:_) = erExperiments
      length experiments `shouldBe` 3 * sampleFileExpNum
      eCheckLast experiment

  describe "GET /experiment/{experimentId}/file" $ do
    it "allows to download experiment data file" $ \env -> do
      file <- apiTry env $ deGetExperimentFile dClient eId
      eCheckFile $ getResponse file

    it "throws a 404 for unknown experiment" $ \env -> do
      apiTry env (deGetExperimentFile dClient unknownSqlId) `shouldThrow`
        errorWithStatus notFound404

  describe "GET /experiment/{experimentId}/metadata" $ do
    it "allows to get experiment metadata" $ \env -> do
      metadata <- apiTry env $ deGetExperimentMetadata dClient eId
      eCheckMetadata metadata

    it "throws a 404 for unknown experiment" $ \env -> do
      apiTry env (deGetExperimentMetadata dClient unknownSqlId) `shouldThrow`
        errorWithStatus notFound404
  where
    eId :: ExperimentId
    eId = SqlId 18

    eCheckLast :: Experiment -> IO ()
    eCheckLast (WithId id_ ExperimentResp {..}) = do
      id_ `shouldBe` eId
      erProject `shouldBe` SqlId 2
      erCompound `shouldBe` (SqlId 4, compoundName4)
      erTarget `shouldBe` (SqlId 3, targetName3)
      erMethodology `shouldBe` Nothing
      erSubExperiments `shouldBe` [SqlId 18]
      erPrimarySubExperiment `shouldBe` SqlId 18
      erPrimaryIC50 `shouldBe` Right 51.155146882869694

    eCheckFile :: ExperimentFileBlob -> IO ()
    eCheckFile ExperimentFileBlob {..} = do
      unExperimentFileBlob `shouldBe` sampleFileBlob

    eCheckMetadata :: ExperimentMetadata -> IO ()
    eCheckMetadata ExperimentMetadata {..} = do
      emDescription `shouldBe` sampleDescription
      emFileMetadata `shouldBe` unFileMetadata sampleMetadata

type SubExperiment = WithId 'SubExperimentId SubExperimentResp
type Measurement = WithId 'MeasurementId MeasurementResp

subExperimentSpec :: SpecWith ClientEnv
subExperimentSpec = do
  describe "GET /subExperiment/{subExperimentId}" $ do
    it "allows to get subExperiment metadata" $ \env -> do
      metadata <- apiTry env $ deGetSubExperiment dClient seId
      seCheckLast metadata

    it "throws a 404 for unknown subExperiment" $ \env -> do
      apiTry env (deGetSubExperiment dClient unknownSqlId) `shouldThrow`
        errorWithStatus notFound404

  describe "GET /subExperiment/{subExperimentId}/measurements" $ do
    it "allows to get subExperiment measurements" $ \env -> do
      measurements@(measurement:_) <- apiTry env $
        deGetMeasurements dClient seId
      length measurements `shouldBe` 10
      mCheckFirst measurement

    it "throws a 404 for unknown subExperiment" $ \env -> do
      apiTry env (deGetMeasurements dClient unknownSqlId) `shouldThrow`
        errorWithStatus notFound404

  describe "PUT /subExperiment/name/{subExperimentId}" $ do
    it "allows to update subExperiment name" $ \env -> do
      subExperiment <- apiTry env $ deSetNameSubExp dClient seId "rename"
      -- return sample data back to decrease influence on other tests
      _ <- apiTry env $ deSetNameSubExp dClient seId "qwe"
      seCheckUpdated subExperiment

    it "throws a 404 for unknown subExperiment" $ \env -> do
      apiTry env (deSetNameSubExp dClient unknownSqlId "rename") `shouldThrow`
        errorWithStatus notFound404

  describe "PUT /subExperiment/suspicious/{subExperimentId}" $ do
    it "allows to change subExperiment suspicious flag" $ \env -> do
      subExperiment <- apiTry env $ deSetIsSuspiciousSubExp dClient seId True
      -- return sample data back to decrease influence on other tests
      _ <- apiTry env $ deSetIsSuspiciousSubExp dClient seId False
      seCheckSuspicious subExperiment

    it "throws a 404 for unknown subExperiment" $ \env -> do
      apiTry env (deSetIsSuspiciousSubExp dClient unknownSqlId True) `shouldThrow`
        errorWithStatus notFound404

  describe "POST /subExperiment/primary/{subExperimentId}" $ do
    it "allows to mark subExperiment as primary" $ \env -> do
      metadata <- apiTry env $ deMakePrimarySubExp dClient sepId
      seCheckPrimary metadata

    it "throws a 404 for unknown subExperiment" $ \env -> do
      apiTry env (deMakePrimarySubExp dClient unknownSqlId) `shouldThrow`
        errorWithStatus notFound404

  describe "POST /subExperiment/{subExperimentId}/new/analyse" $ do
    it "allows to analyse a new subExperiment" $ \env -> do
      result <- apiTry env $ deAnalyseNewSubExp dClient seId seReq
      seCheckAnalyse result

    it "throws a 404 for unknown subExperiment" $ \env -> do
      apiTry env (deAnalyseNewSubExp dClient unknownSqlId seReq) `shouldThrow`
        errorWithStatus notFound404

  describe "POST /subExperiment/{subExperimentId}/new" $ do
    it "allows to create a new subExperiment" $ \env -> do
      experiment <- apiTry env $ deNewSubExp dClient seId seReq
      seCheckNew experiment

    it "throws a 404 for unknown subExperiment" $ \env -> do
      apiTry env (deNewSubExp dClient unknownSqlId seReq) `shouldThrow`
        errorWithStatus notFound404

  describe "DELETE /subExperiment/{subExperimentId}" $ do
    it "allows to delete subExperiment" $ \env -> do
      _ <- apiTry env $ deDeleteSubExp dClient seId
      apiTry env (deGetSubExperiment dClient seId) `shouldThrow`
        errorWithStatus notFound404

    it "throws a 400 for primary subExperiment" $ \env -> do
      apiTry env (deDeleteSubExp dClient sepId) `shouldThrow`
        errorWithStatus badRequest400

    it "throws a 404 for unknown subExperiment" $ \env -> do
      apiTry env (deDeleteSubExp dClient unknownSqlId) `shouldThrow`
        errorWithStatus notFound404
  where
    seId, sepId, senId :: SubExperimentId
    seId = SqlId 19
    sepId = SqlId 1
    senId = SqlId 20

    seCheck :: (SubExperimentId, Text, Bool) -> SubExperiment -> IO ()
    seCheck (sei, sen, ses) (WithId id_ SubExperimentResp{..}) = do
      id_ `shouldBe` sei
      serName `shouldBe` sen
      serIsSuspicious `shouldBe` ses
      seCheckAnalyse serResult

    seCheckLast, seCheckUpdated, seCheckSuspicious, seCheckPrimary, seCheckNew
      :: SubExperiment -> IO ()
    seCheckLast = seCheck (seId, "qwe", False)
    seCheckUpdated = seCheck (seId, "rename", False)
    seCheckSuspicious = seCheck (seId, "qwe", True )
    seCheckPrimary = seCheck (sepId, "Primary", False)
    seCheckNew = seCheck (senId, nserName seReq, False)

    mId :: MeasurementId
    mId = SqlId 1

    mSample :: MeasurementResp
    mSample = MeasurementResp
      { mrConcentration = mConcentration m1
      , mrSignal = mSignal m1
      , mrIsEnabled = True
      }

    mCheckFirst :: Measurement -> IO ()
    mCheckFirst (WithId id_ response) = do
      id_ `shouldBe` mId
      response `shouldBe` mSample

    seReq :: NewSubExperimentReq
    seReq = NewSubExperimentReq
      { nserName = "newSubExperiment"
      , nserChanges = one mId
      }

    -- we do not check exact values here because we have such a tests in
    -- DashboardSpec already
    seCheckAnalyse :: AnalysisResult -> IO ()
    seCheckAnalyse response = response `shouldSatisfy` isRight


dClient :: RunClient m => DashboardEndpoints (AsClientT m)
dClient = genericClient
