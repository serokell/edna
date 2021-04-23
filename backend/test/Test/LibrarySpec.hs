-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Tests for the Library service.

module Test.LibrarySpec
  ( spec
  ) where

import Universum

import qualified Data.Map.Strict as Map

import RIO (runRIO)
import Test.Hspec (Spec, SpecWith, beforeAllWith, describe, it, shouldBe, shouldThrow)

import Edna.Library.Error (LibraryError(..))
import Edna.Library.Service
  (addMethodology, addProject, deleteMethodology, editChemSoft, getCompound, getCompounds,
  getMethodologies, getMethodology, getProject, getProjects, getTarget, getTargets,
  updateMethodology, updateProject)
import Edna.Library.Web.Types
  (CompoundResp(..), MethodologyReq(..), MethodologyResp(..), ProjectReq(..),
  ProjectResp(prCompoundNames, prDescription, prName), TargetResp(..))
import Edna.Setup (EdnaContext)
import Edna.Util (CompoundId, IdType(..), MethodologyId, ProjectId, SqlId(..), TargetId)
import Edna.Web.Types (WithId(..))

import Test.SampleData
import Test.Setup (runTestEdna, runWithInit, withContext)
import Test.Util (methodologyReqToResp)

spec :: Spec
spec = withContext $ do
  beforeAllWith (\ctx -> ctx <$ runWithInit ctx addSampleData) $ do
    describe "getters" gettersSpec
    describe "addition" additionSpec
    describe "modification" modificationSpec
    describe "deletion" deletionSpec
  where
    addSampleData = do
      addSampleProjects
      addSampleMethodologies
      uploadFileTest (SqlId 1) (SqlId 1) sampleFile
      uploadFileTest (SqlId 2) (SqlId 2) sampleFile2

gettersSpec :: SpecWith EdnaContext
gettersSpec = do
  describe "getTarget" $ do
    it "successfully gets known targets one by one" $ runTestEdna $ do
      targets <- mapM getTarget targetIds
      checkTargets targets
    it "fails for unknown target" $ \ctx -> do
      runRIO ctx (getTarget unknownSqlId) `shouldThrow`
        (== LETargetNotFound unknownSqlId)
  describe "getTargets" $ do
    it "successfully gets all targets" $ runTestEdna $ do
      targets <- getTargets Nothing Nothing Nothing
      checkTargets targets

  describe "getCompound" $ do
    it "successfully gets known compounds one by one" $ runTestEdna $ do
      compounds <- mapM getCompound compoundIds
      checkCompounds compounds
    it "fails for unknown compound" $ \ctx -> do
      runRIO ctx (getCompound unknownSqlId) `shouldThrow`
        (== LECompoundNotFound unknownSqlId)
  describe "getCompounds" $ do
    it "successfully gets all compounds" $ runTestEdna $ do
      compounds <- getCompounds Nothing Nothing Nothing
      checkCompounds compounds

  describe "getMethodology" $ do
    it "successfully gets known methodologies one by one" $ runTestEdna $ do
      methodologies <- mapM getMethodology methodologyIds
      checkMethodologies methodologies
    it "fails for unknown methodology" $ \ctx -> do
      runRIO ctx (getMethodology unknownSqlId) `shouldThrow`
        (== LEMethodologyNotFound unknownSqlId)
  describe "getMethodologies" $ do
    it "successfully gets all methodologies" $ runTestEdna $ do
      methodologies <- getMethodologies Nothing Nothing Nothing
      checkMethodologies methodologies

  describe "getProject" $ do
    it "successfully gets known methodologies one by one" $ runTestEdna $ do
      projects <- mapM getProject projectIds
      checkProjects projects
    it "fails for unknown project" $ \ctx -> do
      runRIO ctx (getProject unknownSqlId) `shouldThrow`
        (== LEProjectNotFound unknownSqlId)
  describe "getProjects" $ do
    it "successfully gets all projects" $ runTestEdna $ do
      projects <- getProjects Nothing Nothing Nothing
      checkProjects projects
  where
    -- The way IDs are assigned is implementation detail that we (ab)use here.
    targetIds :: [TargetId]
    targetIds = map SqlId [1, 2, 3, 5]

    compoundIds :: [CompoundId]
    compoundIds = map SqlId [1, 2, 3, 4, 7]

    methodologyIds :: [MethodologyId]
    methodologyIds = map SqlId [1, 2]

    projectIds :: [ProjectId]
    projectIds = map SqlId [1, 2]

    checkTargets :: MonadIO m => [WithId 'TargetId TargetResp] -> m ()
    checkTargets pairs = liftIO $ do
      length pairs `shouldBe` length targetIds
      forM_ pairs $ \WithId {..} -> do
        let
          (expectedName, expectedProjectNames) =
            expectedTargets Map.! unSqlId wiId
        trName wItem `shouldBe` expectedName
        trProjects wItem `shouldBe` expectedProjectNames

    expectedTargets = Map.fromList
      [ (1, (targetName1, [projectName1, projectName2]))
      , (2, (targetName2, [projectName1]))
      , (3, (targetName3, [projectName1]))
      , (5, (targetName4, [projectName2]))
      ]

    checkCompounds :: MonadIO m => [WithId 'CompoundId CompoundResp] -> m ()
    checkCompounds pairs = liftIO $ do
      length pairs `shouldBe` length compoundIds
      forM_ pairs $ \WithId {..} -> do
        let
          expectedName = expectedCompounds Map.! unSqlId wiId
        crName wItem `shouldBe` expectedName
        crChemSoft wItem `shouldBe` Nothing

    expectedCompounds = Map.fromList
      [ (1, compoundName1)
      , (2, compoundName2)
      , (3, compoundName3)
      , (4, compoundName4)
      , (7, compoundName5)
      ]

    checkMethodologies :: MonadIO m => [WithId 'MethodologyId MethodologyResp] -> m ()
    checkMethodologies pairs = liftIO $ do
      length pairs `shouldBe` length methodologyIds
      forM_ pairs $ \WithId {..} -> do
        let
          (expectedName, expectedDescription, expectedConfluence, expectedProj) =
            expectedMethodologies Map.! unSqlId wiId
        mrName wItem `shouldBe` expectedName
        mrDescription wItem `shouldBe` expectedDescription
        mrConfluence wItem `shouldBe` expectedConfluence
        mrProjects wItem `shouldBe` [expectedProj]

    expectedMethodologies = Map.fromList
      [ (1, (methodologyName1, methodologyDescription1, methodologyConfluence1, projectName1))
      , (2, (methodologyName2, methodologyDescription2, methodologyConfluence2, projectName2))
      ]

    checkProjects :: MonadIO m => [WithId 'ProjectId ProjectResp] -> m ()
    checkProjects pairs = liftIO $ do
      length pairs `shouldBe` length projectIds
      forM_ pairs $ \WithId {..} -> do
        let
          (expectedName, expectedDescription, expectedCompoundNames) =
            expectedProjects Map.! unSqlId wiId
        prName wItem `shouldBe` expectedName
        prDescription wItem `shouldBe` expectedDescription
        prCompoundNames wItem `shouldBe` expectedCompoundNames

    expectedProjects = Map.fromList
      [ (1, (projectName1, projectDescription1,
        [compoundName1, compoundName2, compoundName3, compoundName4]))
      , (2, (projectName2, projectDescription2,
        [compoundName1, compoundName2, compoundName5]))
      ]

additionSpec :: SpecWith EdnaContext
additionSpec = do
  describe "addMethodology" $ do
    it "successfully adds methodology with a new name" $ runTestEdna $ do
      let
        mrq = MethodologyReq
          { mrqName = "new"
          , mrqDescription = Just "smth"
          , mrqConfluence = Just sampleURI
          }
      withId <- addMethodology mrq
      liftIO $ wItem withId `shouldBe` methodologyReqToResp mrq []
      liftIO . shouldBe withId =<< getMethodology (wiId withId)
    it "fails to add methodology with an already used name" $ \ctx -> do
      let
        mrq = MethodologyReq
          { mrqName = methodologyName1
          , mrqDescription = Nothing
          , mrqConfluence = Nothing
          }
      runRIO ctx (addMethodology mrq) `shouldThrow`
        (== LEMethodologyNameExists (mrqName mrq))
  describe "addProject" $ do
    it "successfully adds project with a new name" $ runTestEdna $ do
      let
        prq = ProjectReq
          { prqName = "new"
          , prqDescription = Just "smth"
          }
      withId <- addProject prq
      liftIO $ do
        prName (wItem withId) `shouldBe` prqName prq
        prDescription (wItem withId) `shouldBe` prqDescription prq
      liftIO . shouldBe withId =<< getProject (wiId withId)
    it "fails to add project with an already used name" $ \ctx -> do
      let
        prq = ProjectReq
          { prqName = projectName1
          , prqDescription = Nothing
          }
      runRIO ctx (addProject prq) `shouldThrow`
        (== LEProjectNameExists (prqName prq))

modificationSpec :: SpecWith EdnaContext
modificationSpec = do
  describe "editChemSoft" $ do
    it "fails to edit an unknown compound" $ \ctx -> do
      runRIO ctx (editChemSoft unknownSqlId sampleURI) `shouldThrow`
        (== LECompoundNotFound unknownSqlId)
    it "successfully edits chemSoft link of a compound" $ runTestEdna $ do
      let
        compoundId :: CompoundId
        compoundId = SqlId 3
      withId <- editChemSoft compoundId sampleURI
      liftIO $ crChemSoft (wItem withId) `shouldBe` Just sampleURI
      liftIO . shouldBe withId =<< getCompound compoundId
  describe "updateMethodology" $ do
    let
      methodologyId :: MethodologyId
      methodologyId = SqlId 2

      mrq :: MethodologyReq
      mrq = MethodologyReq
        { mrqName = "new name"
        , mrqDescription = Just "new description"
        , mrqConfluence = Just sampleURI
        }
    it "fails to update an unknown methodology" $ \ctx -> do
      runRIO ctx (updateMethodology unknownSqlId mrq) `shouldThrow`
        (== LEMethodologyNotFound unknownSqlId)
    it "fails to update name to a duplicate" $ \ctx -> do
      let mrq' = mrq { mrqName = methodologyName1 }
      runRIO ctx (updateMethodology methodologyId mrq') `shouldThrow`
        (== LEMethodologyNameExists methodologyName1)
    it "successfully edits an existing methodology" $ runTestEdna $ do
      projects <- mrProjects . wItem <$> getMethodology methodologyId
      withId <- updateMethodology methodologyId mrq
      liftIO $ wItem withId `shouldBe` methodologyReqToResp mrq projects
      liftIO . shouldBe withId =<< getMethodology methodologyId
  describe "updateProject" $ do
    let
      projectId :: ProjectId
      projectId = SqlId 2

      prq :: ProjectReq
      prq = ProjectReq
        { prqName = "new name"
        , prqDescription = Just "new description"
        }
    it "fails to update an unknown project" $ \ctx -> do
      runRIO ctx (updateProject unknownSqlId prq) `shouldThrow`
        (== LEProjectNotFound unknownSqlId)
    it "fails to update name to a duplicate" $ \ctx -> do
      let prq' = prq { prqName = projectName1 }
      runRIO ctx (updateProject projectId prq') `shouldThrow`
        (== LEProjectNameExists projectName1)
    it "successfully edits an existing project" $ runTestEdna $ do
      withId <- updateProject projectId prq
      liftIO $ prName (wItem withId) `shouldBe` prqName prq
      liftIO . shouldBe withId =<< getProject projectId

deletionSpec :: SpecWith EdnaContext
deletionSpec = do
  describe "deleteMethodology" $ do
    it "fails to delete an unknown methodology" $ \ctx -> do
      runRIO ctx (deleteMethodology unknownSqlId) `shouldThrow`
        (== LEMethodologyNotFound unknownSqlId )
    it "successfully deletes an existing methodology" $ \ctx -> do
      let
        methodologyId :: MethodologyId
        methodologyId = SqlId 2
      void $ runRIO ctx $ deleteMethodology methodologyId
      flip shouldThrow (== LEMethodologyNotFound methodologyId) $
        runRIO ctx $ getMethodology methodologyId
