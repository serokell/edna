-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Tests for the Library service.

module Test.LibrarySpec
  ( spec
  ) where

import Universum

import qualified Data.Map.Strict as Map

import Fmt (pretty)
import RIO (runRIO)
import Servant.Util (asc, desc, fullContent, itemsOnPage, mkSortingSpec, noSorting, skipping)
import Servant.Util.Internal.Util (Positive(..))
import Test.Hspec
  (Expectation, Spec, SpecWith, beforeAllWith, describe, expectationFailure, it, shouldBe,
  shouldThrow)

import Edna.Library.Error (LibraryError(..))
import Edna.Library.Service
  (addMethodology, addProject, deleteMethodology, editChemSoft, editMde, getCompound, getCompounds,
  getMethodologies, getMethodology, getProject, getProjects, getTarget, getTargets,
  updateMethodology, updateProject)
import Edna.Library.Web.Types
  (CompoundResp(..), MethodologyReq(..), MethodologyResp(..), ProjectReq(..), ProjectResp(..),
  TargetResp(..))
import Edna.Setup (EdnaContext)
import Edna.Util (CompoundId, IdType(..), MethodologyId, ProjectId, SqlId(..), TargetId)
import Edna.Util.URI (URI)
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
  let mkPagination (toTake, toDrop) =
        skipping (fromIntegral toDrop) $
        itemsOnPage (PositiveUnsafe $ fromIntegral toTake)

  describe "getTarget" $ do
    it "successfully gets known targets one by one" $ runTestEdna $ do
      targets <- mapM getTarget targetIds
      checkTargets Nothing Nothing targets
    it "fails for unknown target" $ \ctx -> do
      runRIO ctx (getTarget unknownSqlId) `shouldThrow`
        (== LETargetNotFound unknownSqlId)
  describe "getTargets" $ do
    it "successfully gets all targets" $ runTestEdna $ do
      targets <- getTargets noSorting fullContent
      checkTargets Nothing Nothing targets

    it "properly applies sorting and pagination" $ runTestEdna $ do
      let paginationAsc = (2, 1)
      targetsAscName <- getTargets (mkSortingSpec [asc #name])
        (mkPagination paginationAsc)
      checkTargets (Just (compare `on` view (_2 . _1))) (Just paginationAsc) targetsAscName
      let paginationDesc = (3, 0)
      targetsDescDate <- getTargets (mkSortingSpec [desc #additionDate])
        (mkPagination paginationDesc)
      -- here sorting by date is practically equivalent to sorting by ID
      checkTargets (Just (compare `on` (Down . view _1))) (Just paginationDesc) targetsDescDate

  describe "getCompound" $ do
    it "successfully gets known compounds one by one" $ runTestEdna $ do
      compounds <- mapM getCompound compoundIds
      checkCompounds Nothing Nothing compounds
    it "fails for unknown compound" $ \ctx -> do
      runRIO ctx (getCompound unknownSqlId) `shouldThrow`
        (== LECompoundNotFound unknownSqlId)
  describe "getCompounds" $ do
    it "successfully gets all compounds" $ runTestEdna $ do
      compounds <- getCompounds noSorting fullContent
      checkCompounds Nothing Nothing compounds

    it "properly applies sorting and pagination" $ runTestEdna $ do
      let paginationAsc = (4, 2)
      compoundsAsc <- getCompounds (mkSortingSpec [asc #name])
        (mkPagination paginationAsc)
      checkCompounds (Just $ (compare `on` snd)) (Just paginationAsc) compoundsAsc
      let paginationDesc = (8, 4)
      compoundsDesc <- getCompounds (mkSortingSpec [desc #name])
        (mkPagination paginationDesc)
      checkCompounds (Just (compare `on` (Down . snd))) (Just paginationDesc) compoundsDesc

  describe "getMethodology" $ do
    it "successfully gets known methodologies one by one" $ runTestEdna $ do
      methodologies <- mapM getMethodology methodologyIds
      checkMethodologies Nothing Nothing methodologies
    it "fails for unknown methodology" $ \ctx -> do
      runRIO ctx (getMethodology unknownSqlId) `shouldThrow`
        (== LEMethodologyNotFound unknownSqlId)
  describe "getMethodologies" $ do
    it "successfully gets all methodologies" $ runTestEdna $ do
      methodologies <- getMethodologies noSorting fullContent
      checkMethodologies Nothing Nothing methodologies

    it "properly applies sorting and pagination" $ runTestEdna $ do
      let paginationAsc = (1, 0)
      methodologiesAsc <- getMethodologies (mkSortingSpec [asc #name])
        (mkPagination paginationAsc)
      checkMethodologies (Just (compare `on` (view $ _2 . _1)))
        (Just paginationAsc) methodologiesAsc
      let paginationDesc = (1, 1)
      methodologiesDesc <- getMethodologies (mkSortingSpec [desc #name])
        (mkPagination paginationDesc)
      checkMethodologies (Just (compare `on` (Down . view (_2 . _1))))
        (Just paginationDesc) methodologiesDesc

  describe "getProject" $ do
    it "successfully gets known methodologies one by one" $ runTestEdna $ do
      projects <- mapM getProject projectIds
      checkProjects Nothing Nothing projects
    it "fails for unknown project" $ \ctx -> do
      runRIO ctx (getProject unknownSqlId) `shouldThrow`
        (== LEProjectNotFound unknownSqlId)
  describe "getProjects" $ do
    it "successfully gets all projects" $ runTestEdna $ do
      projects <- getProjects noSorting fullContent
      checkProjects Nothing Nothing projects

    it "properly applies sorting and pagination" $ runTestEdna $ do
      let paginationAsc = (1, 0)
      methodologiesAsc <- getProjects (mkSortingSpec [asc #name])
        (mkPagination paginationAsc)
      checkProjects (Just (compare `on` (view $ _2 . _1)))
        (Just paginationAsc) methodologiesAsc
      let paginationDesc = (1, 1)
      methodologiesDesc <- getProjects (mkSortingSpec [desc #creationDate])
        (mkPagination paginationDesc)
      checkProjects (Just (compare `on` (Down . fst)))
        (Just paginationDesc) methodologiesDesc
  where
    -- The way IDs are assigned is implementation detail that we (ab)use here.
    targetIds :: [TargetId]
    targetIds = map (SqlId . fst) allExpectedTargets

    compoundIds :: [CompoundId]
    compoundIds = map (SqlId . fst) allExpectedCompounds

    methodologyIds :: [MethodologyId]
    methodologyIds = map (SqlId . fst) allExpectedMethodologies

    projectIds :: [ProjectId]
    projectIds = map (SqlId . fst) allExpectedProjects

    checkPairs :: MonadIO m =>
      [(Word32, a)] -> ((item, a) -> Expectation) ->
      Maybe ((Word32, a) -> (Word32, a) -> Ordering) -> Maybe (Int, Int) ->
      [WithId idTag item] -> m ()
    checkPairs allExpectedItems checkItem mSortCmp mTakeDrop pairs = liftIO $ do
      let expected =
            maybe id (\(toTake, toDrop) -> take toTake . drop toDrop) mTakeDrop .
            maybe id sortBy mSortCmp $
            allExpectedItems
      let expectedMap = Map.fromList expected
      length pairs `shouldBe` length expected
      forM_ pairs $ \WithId {..} ->
        case expectedMap Map.!? unSqlId wiId of
          Nothing -> expectationFailure $ "couldn't find item " <> pretty wiId
          Just a -> checkItem (wItem, a)

    checkTargets :: MonadIO m =>
      Maybe ((Word32, (Text, [Text])) -> (Word32, (Text, [Text])) -> Ordering) ->
      Maybe (Int, Int) ->
      [WithId 'TargetId TargetResp] -> m ()
    checkTargets = checkPairs allExpectedTargets $
      \(TargetResp {..}, (expectedName, expectedProjectNames)) -> do
        trName `shouldBe` expectedName
        trProjects `shouldBe` expectedProjectNames

    allExpectedTargets =
      [ (1, (targetName1, [projectName1, projectName2]))
      , (2, (targetName2, [projectName1]))
      , (3, (targetName3, [projectName1]))
      , (5, (targetName4, [projectName2]))
      ]

    checkCompounds :: MonadIO m =>
      Maybe ((Word32, Text) -> (Word32, Text) -> Ordering) ->
      Maybe (Int, Int) ->
      [WithId 'CompoundId CompoundResp] -> m ()
    checkCompounds = checkPairs allExpectedCompounds $
      \(CompoundResp {..}, expectedName) -> do
        crName `shouldBe` expectedName
        crChemSoft `shouldBe` Nothing
        crMde `shouldBe` Nothing

    allExpectedCompounds =
      [ (1, compoundName1)
      , (2, compoundName2)
      , (3, compoundName3)
      , (4, compoundName4)
      , (7, compoundName5)
      ]

    checkMethodologies :: MonadIO m =>
      Maybe ((Word32, (Text, Maybe Text, Maybe URI, Text)) ->
             (Word32, (Text, Maybe Text, Maybe URI, Text)) ->
              Ordering) ->
      Maybe (Int, Int) ->
      [WithId 'MethodologyId MethodologyResp] -> m ()
    checkMethodologies = checkPairs allExpectedMethodologies $
      \(MethodologyResp {..},
        (expectedName, expectedDescription, expectedConfluence, expectedProj)) -> do
          mrName `shouldBe` expectedName
          mrDescription `shouldBe` expectedDescription
          mrConfluence `shouldBe` expectedConfluence
          mrProjects `shouldBe` [expectedProj]

    allExpectedMethodologies =
      [ (1, (methodologyName1, methodologyDescription1, methodologyConfluence1, projectName1))
      , (2, (methodologyName2, methodologyDescription2, methodologyConfluence2, projectName2))
      ]

    checkProjects :: MonadIO m =>
      Maybe ((Word32, (Text, Maybe Text, [Text])) ->
             (Word32, (Text, Maybe Text, [Text])) ->
              Ordering) ->
      Maybe (Int, Int) ->
      [WithId 'ProjectId ProjectResp] -> m ()
    checkProjects = checkPairs allExpectedProjects $
      \(ProjectResp {..},
        (expectedName, expectedDescription, expectedCompoundNames)) -> do
          prName `shouldBe` expectedName
          prDescription `shouldBe` expectedDescription
          prCompoundNames `shouldBe` expectedCompoundNames

    allExpectedProjects =
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
  describe "editCompoundLinks" $ do
    it "fails to edit an unknown compound" $ \ctx -> do
      runRIO ctx (editChemSoft unknownSqlId sampleURI) `shouldThrow`
        (== LECompoundNotFound unknownSqlId)
    it "successfully edits links of a compound" $ runTestEdna $ do
      let
        compoundId :: CompoundId
        compoundId = SqlId 3
      _ <- editChemSoft compoundId sampleURI
      withId <- editMde compoundId sampleURI
      liftIO $ crChemSoft (wItem withId) `shouldBe` Just sampleURI
      liftIO $ crMde (wItem withId) `shouldBe` Just sampleURI
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
