module Test.DB.DBSpec
  ( spec
  ) where

import Universum

import Database.Beam (all_, insert, insertValues, select)
import Database.Beam.Backend (SqlSerial(..))
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (forAll, modifyMaxShrinks, modifyMaxSuccess, (===))

import Edna.DB.Integration (runInsert', runSelectReturningList', runSelectReturningOne')
import Edna.DB.Schema
  (AnalysisMethodT(..), CompoundT(..), EdnaSchema(..), ExperimentFileT(..), ExperimentT(..),
  MeasurementT(..), ProjectT(..), SubExperimentT(..), TargetT(..), TestMethodologyT(..), ednaSchema,
  theOnlyAnalysisMethod, theOnlyAnalysisMethodId)
import Test.DB.Gen
  (genAnalysisMethodRec, genCompoundRec, genExperimentFileRec, genExperimentRec, genMeasurementRec,
  genProjectRec, genRemovedMeasurementsRec, genSubExperimentRec, genTargetRec,
  genTestMethodologyRec)
import Test.Setup (ednaTestMode, withContext)

spec :: Spec
spec = withContext $
  -- We (almost) disable shrinking because when there are many shrinks something
  -- goes wrong and we often get unexpected errors from SQL, like this one:
  -- > insert or update on table "experiment" violates foreign key constraint "has_experiment_file"
  describe "Validate schema" $ modifyMaxSuccess (min 20) $ modifyMaxShrinks (const 3) $ do
    let EdnaSchema {..} = ednaSchema
    let insertWithConflict schema value = runInsert' $ insert schema value
    let commonCheck schema expected = do
          actual <- lift $ runSelectReturningOne' $ select $ all_ schema
          actual === Just expected
    let insertProject = do
          expectedProject <- forAll $ genProjectRec 1
          lift $ insertWithConflict esProject $ insertValues [expectedProject]
          pure expectedProject
    let insertTestMethodology = do
          expectedMethodology <- forAll $ genTestMethodologyRec 1
          lift $ insertWithConflict esTestMethodology $ insertValues [expectedMethodology]
          pure expectedMethodology
    let insertTarget = do
          expectedTarget <- forAll $ genTargetRec 1
          lift $ insertWithConflict esTarget $ insertValues [expectedTarget]
          pure expectedTarget
    let insertCompound = do
          expectedCompound <- forAll $ genCompoundRec 1
          lift $ insertWithConflict esCompound $ insertValues [expectedCompound]
          pure expectedCompound
    let insertExperimentFile = do
          ProjectRec{..} <- insertProject
          TestMethodologyRec{..} <- insertTestMethodology
          expectedExperimentFile <- forAll $ genExperimentFileRec 1
            (unSerial pProjectId) (unSerial tmTestMethodologyId)
          lift $ insertWithConflict esExperimentFile $ insertValues [expectedExperimentFile]
          pure expectedExperimentFile
    let insertExperiment = do
          ExperimentFileRec{..} <- insertExperimentFile
          CompoundRec{..} <- insertCompound
          TargetRec{..} <- insertTarget
          let expectedExperiment = genExperimentRec 1
                (unSerial efExperimentFileId) (unSerial cCompoundId) (unSerial tTargetId)
          lift $ insertWithConflict esExperiment $ insertValues [expectedExperiment]
          pure expectedExperiment
    let insertMeasurement = do
          ExperimentRec{..} <- insertExperiment
          expectedMeasurement <- forAll $ genMeasurementRec 1 $ unSerial eExperimentId
          lift $ insertWithConflict esMeasurement $ insertValues [expectedMeasurement]
          pure expectedMeasurement
    let insertAnalysisMethod = do
          -- Analysis method with ID 1 is added by default.
          -- We never add a new one in the current version, but we will do it
          -- one day and we want to check that our types are accurate.
          -- So we add it with ID 2.
          expectedAnalysisMethod <-
            forAll $ genAnalysisMethodRec $ succ theOnlyAnalysisMethodId
          lift $ insertWithConflict esAnalysisMethod $ insertValues [expectedAnalysisMethod]
          pure expectedAnalysisMethod
    let insertSubExperiment = do
          AnalysisMethodRec{..} <- insertAnalysisMethod
          ExperimentRec{..} <- insertExperiment
          expectedSubExperiment <- forAll $ genSubExperimentRec 1
            (unSerial amAnalysisMethodId) (unSerial eExperimentId)
          lift $ insertWithConflict esSubExperiment $ insertValues [expectedSubExperiment]
          pure expectedSubExperiment
    let insertRemovedMeasurements = do
          SubExperimentRec{..} <- insertSubExperiment
          m@MeasurementRec{..} <- forAll $ genMeasurementRec 1 seExperimentId
          lift $ insertWithConflict esMeasurement $ insertValues [m]
          let expectedRemovedMeasurements = genRemovedMeasurementsRec
                (unSerial seSubExperimentId) (unSerial mMeasurementId)
          lift $ insertWithConflict esRemovedMeasurements $ insertValues [expectedRemovedMeasurements]
          pure expectedRemovedMeasurements
    it "Project" $ \ctx -> ednaTestMode ctx $
      insertProject >>= commonCheck esProject
    it "TestMethodology" $ \ctx -> ednaTestMode ctx $
      insertTestMethodology >>= commonCheck esTestMethodology
    it "Target" $ \ctx -> ednaTestMode ctx $
      insertTarget >>= commonCheck esTarget
    it "Compound" $ \ctx -> ednaTestMode ctx $
      insertCompound >>= commonCheck esCompound
    it "ExperimentFile" $ \ctx -> ednaTestMode ctx $
      insertExperimentFile >>= commonCheck esExperimentFile
    it "Experiment" $ \ctx -> ednaTestMode ctx $
      insertExperiment >>= commonCheck esExperiment
    it "Measurement" $ \ctx -> ednaTestMode ctx $
      insertMeasurement >>= commonCheck esMeasurement
    it "AnalysisMethod" $ \ctx -> ednaTestMode ctx $ do
      expected2 <- insertAnalysisMethod
      actual <- lift $ runSelectReturningList' $ select $ all_ esAnalysisMethod
      actual === [theOnlyAnalysisMethod, expected2]
    it "SubExperiment" $ \ctx -> ednaTestMode ctx $
      insertSubExperiment >>= commonCheck esSubExperiment
    it "RemovedMeasurements" $ \ctx -> ednaTestMode ctx $
      insertRemovedMeasurements >>= commonCheck esRemovedMeasurements
