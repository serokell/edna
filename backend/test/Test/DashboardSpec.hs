-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Tests for the Dashboard service.

module Test.DashboardSpec
  ( spec
  ) where

import Universum

import qualified Data.Map.Strict as Map

import RIO (runRIO)
import Servant.Util (asc, desc, fullContent, itemsOnPage, mkSortingSpec, noSorting, skipping)
import Servant.Util.Dummy.Pagination (paginate)
import Servant.Util.Internal.Util (Positive(..))
import Test.Hspec
  (Spec, SpecWith, beforeAllWith, describe, it, shouldBe, shouldSatisfy, shouldThrow)

import qualified Edna.Library.Service as Library

import Edna.Analysis.FourPL (Params4PL(..), analyse4PLOne)
import Edna.Dashboard.Error (DashboardError(..))
import Edna.Dashboard.Service
  (analyseNewSubExperiment, deleteSubExperiment, getExperimentFile, getExperimentMetadata,
  getExperiments, getMeasurements, getSubExperiment, makePrimarySubExperiment, newSubExperiment,
  setIsSuspiciousSubExperiment, setNameSubExperiment)
import Edna.Dashboard.Web.Types
  (ExperimentFileBlob(..), ExperimentMetadata(..), ExperimentResp(..), ExperimentsResp(..),
  MeasurementResp(..), NewSubExperimentReq(..), SubExperimentResp(..))
import Edna.ExperimentReader.Types
  (FileMetadata(unFileMetadata), Measurement(..), measurementToPairMaybe)
import Edna.Library.Web.Types (MethodologyReq(..))
import Edna.Setup (EdnaContext)
import Edna.Util (ExperimentId, IdType(..), SqlId(..), SubExperimentId)
import Edna.Web.Types (WithId(..))

import Test.Orphans ()
import Test.SampleData
import Test.Setup (runTestEdna, runWithInit, withContext)
import Test.Util (DefaultPgNullsOrder(..))

spec :: Spec
spec = withContext $ do
  beforeAllWith (\ctx -> ctx <$ runWithInit ctx addSampleData) $ do
    gettersSpec
    describe "makePrimarySubExperiment" $ do
      it "makes given sub-experiment the primary one for its parent" $ runTestEdna do
        resp <- makePrimarySubExperiment secondarySubExpId
        getResp <- getSubExperiment secondarySubExpId
        ExperimentsResp {..} <- getExperiments
          (Just $ SqlId 1) Nothing Nothing noSorting fullContent
        let WithId _ ExperimentResp {..}: _ =
              filter (\withId -> wiId withId == SqlId 1) erExperiments
        liftIO $ do
          resp `shouldBe` getResp
          erPrimarySubExperiment `shouldBe` secondarySubExpId
        void $ makePrimarySubExperiment sampleSubExpId -- undo changes
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
      it "fails for unknown sub-experiment" $ \ctx -> do
        runRIO ctx (deleteSubExperiment unknownSqlId) `shouldThrow`
          (== DESubExperimentNotFound unknownSqlId)
      it "successfully deletes non-primary sub-experiment" $ \ctx -> do
        runRIO ctx (deleteSubExperiment secondarySubExpId >> getSubExperiment secondarySubExpId)
          `shouldThrow`
          (== DESubExperimentNotFound secondarySubExpId)
    describe "newSubExperiment & analyseNewSubExperiment" $ do
      it "successfully computes and creates a new sub-experiment" $ runTestEdna $ do
        let changedMeasurement = 1
        let sourceSubExp = SqlId 1
        let nser = NewSubExperimentReq
              { nserName = "newSubExperiment"
              , nserChanges = one (SqlId changedMeasurement)
              }
        let
          step :: Word32 -> Measurement -> WithId 'MeasurementId MeasurementResp
          step i m = WithId (SqlId i) MeasurementResp
            { mrConcentration = mConcentration m
            , mrSignal = mSignal m
            , mrIsEnabled = (i == changedMeasurement) == mIsOutlier m
            }
        let expectedMeasurements = zipWith step [1..] [m1, m2, m3, m4, m5, m6, m7, m8, m9, m10]

        let activePoints = flip mapMaybe expectedMeasurements $ \(wItem -> measResp) ->
              guard (mrIsEnabled measResp) $> (mrConcentration measResp, mrSignal measResp)

        (analysedRemovals, analysedResult) <-
          analyseNewSubExperiment sourceSubExp nser
        WithId newId resp <- newSubExperiment sourceSubExp nser
        WithId _ resp' <- getSubExperiment newId
        measurements <- getMeasurements newId
        params4PL <- analyse4PLOne activePoints
        liftIO $ do
          analysedResult `shouldBe` params4PL
          analysedRemovals `shouldBe` [SqlId changedMeasurement]

          resp `shouldBe` resp'
          serName resp `shouldBe` nserName nser
          serIsSuspicious resp `shouldBe` False
          serResult resp `shouldBe` params4PL
          measurements `shouldBe` expectedMeasurements
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

    sampleSubExpId :: SubExperimentId
    sampleSubExpId = SqlId 1

    -- non-primary
    secondarySubExpId :: SubExperimentId
    secondarySubExpId = SqlId (3 * sampleFileExpNum + 1)

gettersSpec :: SpecWith EdnaContext
gettersSpec = do
  describe "getters" $ do
    describe "getExperiments" $ do
      it "returns all experiments with no filters" $ runTestEdna $ do
        ExperimentsResp {..} <- getExperiments Nothing Nothing Nothing
          noSorting fullContent
        liftIO $ do
          length erExperiments `shouldBe` 3 * sampleFileExpNum
      it "filters by project correctly" $ runTestEdna $ do
        ExperimentsResp {..} <- getExperiments (Just $ SqlId 1) Nothing Nothing
          noSorting fullContent
        liftIO $ do
          length erExperiments `shouldBe` sampleFileExpNum
          forM_ erExperiments $ \(WithId _ ExperimentResp {..}) ->
            erProject `shouldBe` SqlId 1
      it "filters by project and compound correctly" $ runTestEdna $ do
        let compoundId = SqlId 2
        ExperimentsResp {..} <-
          getExperiments (Just $ SqlId 1) (Just compoundId) Nothing
            noSorting fullContent
        liftIO $ do
          length erExperiments `shouldBe` 2
          forM_ erExperiments $ \(WithId _ ExperimentResp {..}) ->
            fst erCompound `shouldBe` compoundId
      it "filters by 3 filters correctly and returns mean IC50" $ runTestEdna $ do
        let compoundId = SqlId 2
        let targetId = SqlId 2
        let measurements = targetMeasurements2 Map.! compoundName2
        ExperimentsResp {..} <-
          getExperiments (Just $ SqlId 1) (Just compoundId) (Just targetId)
            noSorting fullContent
        Right Params4PL {..} <- analyse4PLOne (mapMaybe measurementToPairMaybe measurements)
        liftIO $ do
          length erExperiments `shouldBe` 1
          -- It's checked above for better error message
          let [WithId _ expResp] = erExperiments
          erPrimaryIC50 expResp `shouldBe` Right p4plC
          forM_ erExperiments $ \(WithId _ ExperimentResp {..}) -> do
            erTarget `shouldBe` (targetId, targetName2)
            erCompound `shouldBe` (compoundId, compoundName2)
      it "filters by project and properly applies sorting and pagination" $ runTestEdna $ do
        let
          size :: Num n => n
          size = sampleFileExpNum + 1
          projectId = SqlId 2
        ExperimentsResp {..} <- getExperiments (Just projectId) Nothing Nothing
          (mkSortingSpec [desc #uploadDate]) (skipping 2 $ itemsOnPage (PositiveUnsafe size))
        liftIO $ do
          length erExperiments `shouldBe` size
          forM_ erExperiments $ \(WithId _ ExperimentResp {..}) ->
            erProject `shouldBe` projectId
          let dates = map (erUploadDate . wItem) erExperiments
          sortWith Down dates `shouldBe` dates
      it "properly sorts all experiments by various fields" $ runTestEdna $ do
        let
          size :: Num n => n
          size = sampleFileExpNum + 1
        let paginationSpec = skipping 3 $ itemsOnPage (PositiveUnsafe size)
        let getExperiments' sorting pagination = erExperiments <$>
              getExperiments Nothing Nothing Nothing sorting pagination
        allExperiments <- getExperiments' noSorting fullContent
        let getSortedIds sorting = map wiId <$> getExperiments' sorting paginationSpec
        let
          paginateAndGetIds :: [WithId a b] -> [SqlId a]
          paginateAndGetIds = map wiId . paginate paginationSpec

        descByMethodology <- getSortedIds (mkSortingSpec [desc #methodology])
        ascByCompound <- getSortedIds (mkSortingSpec [asc #compound])
        descByTarget <- getSortedIds (mkSortingSpec [desc #target])
        liftIO $ do
          let alsoSortById f (WithId sqlId er) = (f er, Down sqlId)
          let getMethodologyName = alsoSortById $
                Down . DefaultPgNullsOrder . fmap snd . erMethodology
          let getCompoundName = alsoSortById $ snd . erCompound
          let getTargetName = alsoSortById $ Down . snd . erTarget
          descByMethodology `shouldBe`
            paginateAndGetIds (sortWith getMethodologyName allExperiments)
          ascByCompound `shouldBe`
            paginateAndGetIds (sortWith getCompoundName allExperiments)
          descByTarget `shouldBe`
            paginateAndGetIds (sortWith getTargetName allExperiments)

    describe "getExperimentMetadata" $ do
      it "returns correct metadata for all known experiments" $ runTestEdna $ do
        forM_ validExperimentIds $ \expId -> do
          let expectedMetadata =
                ExperimentMetadata sampleDescription (unFileMetadata sampleMetadata)
          metadata <- getExperimentMetadata expId
          liftIO $ metadata `shouldBe` expectedMetadata
      it "fails for unknown experiment" $ \ctx -> do
        runRIO ctx (getExperimentMetadata unknownSqlId) `shouldThrow`
          (== DEExperimentNotFound unknownSqlId)
    describe "getExperimentFile" $ do
      it "returns correct file name and blob for all known experiments" $ runTestEdna $ do
        forM_ validExperimentIds $ \expId -> do
          let expectedPair =
                (sampleFileName, ExperimentFileBlob sampleFileBlob)
          filePair <- getExperimentFile expId
          liftIO $ filePair `shouldBe` expectedPair
      it "fails for unknown experiment" $ \ctx -> do
        runRIO ctx (getExperimentFile unknownSqlId) `shouldThrow`
          (== DEExperimentNotFound unknownSqlId)
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
        runRIO ctx (getSubExperiment unknownSqlId) `shouldThrow`
          (== DESubExperimentNotFound unknownSqlId)
    describe "getMeasurements" $ do
      it "returns correct measurements for sub-experiments 13-19" $ runTestEdna $ do
        [ measurements1
          , measurements2
          , measurements3
          , measurements4
          , measurements5
          , measurements6
          , measurements7
          ] <- mapM ((wItem <<$>>) . getMeasurements) $
            drop (2 * sampleFileExpNum) validSubExperimentIds
        let
          toMeasurementResp :: Measurement -> MeasurementResp
          toMeasurementResp Measurement {..} = MeasurementResp
            { mrConcentration = mConcentration
            , mrSignal = mSignal
            , mrIsEnabled = not mIsOutlier
            }
        -- These lists match lists in @targetMeasurementsX@ values from SampleData.
        liftIO $ do
          measurements1 `shouldBe` map toMeasurementResp [m1, m2, m3, m4, m5, m6, m7, m8, m9, m10]
          measurements2 `shouldBe` map toMeasurementResp [m1, m3, m4, m5Outlier, m6, m7, m9, m10]
          measurements3 `shouldBe` map toMeasurementResp [m1, m2, m3, m4, m5Outlier, m6, m7, m8, m9, m10]
          measurements4 `shouldBe` map toMeasurementResp [m1, m2, m3, m4, m7, m8, m9]
          measurements5 `shouldBe` map toMeasurementResp [m1, m3, m4, m5, m7, m8, m9, m10]
          measurements6 `shouldBe` map toMeasurementResp [m1, m2, m4, m5Outlier, m7, m8, m9, m10]
          measurements7 `shouldBe` measurements1 -- no changes
      it "fails for unknown sub-experiment" $ \ctx -> do
        runRIO ctx (getMeasurements unknownSqlId) `shouldThrow`
          (== DESubExperimentNotFound unknownSqlId)

validSubExperimentIds :: [SubExperimentId]
validSubExperimentIds = map SqlId [1 .. 19]

validExperimentIds :: [ExperimentId]
validExperimentIds = map SqlId [1 .. 18]
