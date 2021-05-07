-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | API testing for Library.

module Test.API.Library
  ( spec
  ) where

import Universum

import Network.HTTP.Types (badRequest400, notFound404)
import Network.Wai.Handler.Warp (testWithApplication)
import Servant.Client (ClientEnv)
import Servant.Client.Core (RunClient)
import Servant.Client.Generic (AsClientT, genericClient)
import Servant.Util (fullContent, noSorting)
import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow)
import Test.Hspec.Core.Runner (Summary, Config, runSpec)

import Edna.Library.Web.API
  (CompoundEndpoints(..), MethodologyEndpoints(..), ProjectEndpoints(..), TargetEndpoints(..))
import Edna.Library.Web.Types
  (CompoundResp(..), MethodologyReq(..), MethodologyResp(..), ProjectReq(..), ProjectResp(..),
  TargetResp(..))
import Edna.Util (CompoundId, IdType(..), MethodologyId, ProjectId, SqlId(..), TargetId)
import Edna.Web.Types (URI, WithId(..))

import Test.API.Util (apiTry, app, clientEnv, errorWithStatus)
import Test.Orphans ()
import Test.SampleData
import Test.Setup (runWithInit, withContext)

spec :: Config -> IO Summary
spec config = withContext $ \ctx ->  do
  _ <- ctx <$ runWithInit ctx addSampleData
  testWithApplication (app ctx) $ \port -> do
    env <- clientEnv port
    let runner = flip runSpec $ config
    liftIO $ runner $ do
      describe "target API" $ targetSpec env
      describe "compound API" $ compoundSpec env
      describe "methodology API" $ methodologySpec env
      describe "project API" $ projectSpec env
  where
    addSampleData = do
      addSampleProjects
      addSampleMethodologies
      uploadFileTest (SqlId 1) (SqlId 1) sampleFile
      uploadFileTest (SqlId 2) (SqlId 2) sampleFile2


type Target = WithId 'TargetId TargetResp

targetSpec :: ClientEnv -> Spec
targetSpec env = do
  describe "GET /targets" $ do
    it "allows to list all targets" $ do
      targets@(target:_) <- apiTry env $
        teGetTargets tClient noSorting fullContent
      length targets `shouldBe` 4
      tCheckLast target

  describe "GET /target/{targetId}" $ do
    it "allows to get target" $ do
      target <- apiTry env $ teGetTarget tClient tId
      tCheckLast target

    it "throws a 404 for unknown target" $ do
      apiTry env (teGetTarget tClient unknownSqlId) `shouldThrow`
        errorWithStatus notFound404
  where
    tClient :: RunClient m => TargetEndpoints (AsClientT m)
    tClient = genericClient

    tId :: TargetId
    tId = SqlId 5

    tCheck :: (TargetId, Text, [Text]) -> Target -> IO ()
    tCheck (ti, tn, tp) (WithId id_ response) = do
      id_ `shouldBe` ti
      trName response `shouldBe` tn
      trProjects response `shouldBe` tp

    tCheckLast :: Target -> IO ()
    tCheckLast = tCheck (tId, targetName4, [projectName2])


type Compound = WithId 'CompoundId CompoundResp

compoundSpec :: ClientEnv -> Spec
compoundSpec env = do
  describe "GET /compounds" $ do
    it "allows to list all compounds" $ do
      compounds@(compound:_) <- apiTry env $
        ceGetCompounds cClient noSorting fullContent
      length compounds `shouldBe` 5
      cCheckLast compound

  describe "GET /compound/{compoundId}" $ do
    it "allows to get compound" $ do
      compound <- apiTry env $ ceGetCompound cClient cId
      cCheckLast compound

    it "throws a 404 for unknown compound" $ do
      apiTry env (ceGetCompound cClient unknownSqlId) `shouldThrow`
        errorWithStatus notFound404

  describe "PUT /compound/chemsoft{compoundId}" $ do
    it "allows to update chemsoft link of a compound" $ do
      compound <- apiTry env $ ceEditChemSoft cClient cId sampleURI
      cCheckChemSoft compound

    it "throws a 404 for unknown compound" $ do
      apiTry env (ceEditChemSoft cClient unknownSqlId sampleURI) `shouldThrow`
        errorWithStatus notFound404

  describe "/compound/mde/{compoundId}" $ do
    it "allows to update mde link of a compound" $ do
      compound <- apiTry env $ ceEditMde cClient cId sampleURI
      cCheckMde compound

    it "throws a 404 for unknown compound" $ do
      apiTry env (ceEditMde cClient unknownSqlId sampleURI) `shouldThrow`
        errorWithStatus notFound404
  where
    cClient :: RunClient m => CompoundEndpoints (AsClientT m)
    cClient = genericClient

    cId :: CompoundId
    cId = SqlId 7

    cCheck :: (CompoundId, Text, Maybe URI, Maybe URI) -> Compound -> IO ()
    cCheck (ci, cn, cc, cm) (WithId id_ response) = do
      id_ `shouldBe` ci
      crName response `shouldBe` cn
      crChemSoft response `shouldBe` cc
      crMde response `shouldBe` cm

    cCheckLast, cCheckChemSoft, cCheckMde :: Compound -> IO ()
    cCheckLast = cCheck (cId, compoundName5, Nothing, Nothing)
    cCheckChemSoft = cCheck (cId, compoundName5, Just sampleURI, Nothing)
    cCheckMde = cCheck (cId, compoundName5, Just sampleURI, Just sampleURI)


type Methodology = WithId 'MethodologyId MethodologyResp

methodologySpec :: ClientEnv -> Spec
methodologySpec env = do
  describe "GET /methodologies" $ do
    it "allows to list all methodologies" $ do
      methodologies@(methodology:_) <- apiTry env $
        meGetMethodologies mClient noSorting fullContent
      length methodologies `shouldBe` 2
      mCheckLast methodology

  describe "GET /methodology/{methodologyId}" $ do
    it "allows to get methodology" $ do
      methodology <- apiTry env $ meGetMethodology mClient mId
      mCheckLast methodology

    it "throws a 404 for unknown methodology" $ do
      apiTry env (meGetMethodology mClient unknownSqlId) `shouldThrow`
        errorWithStatus notFound404

  describe "POST /methodology" $ do
    it "allows to create methodology" $ do
      methodology <- apiTry env $ meAddMethodology mClient mRq
      mCheckCreated methodology

    it "throws a 400 for try to set not unique name" $ do
      let mRq' = mRq { mrqName = methodologyName1 }
      apiTry env (meAddMethodology mClient mRq') `shouldThrow`
        errorWithStatus badRequest400

  describe "PUT /methodology/{methodologyId}" $ do
    it "allows to update methodology" $ do
      let mRq' = mRq { mrqName = "rename" }
      methodology <- apiTry env $ meEditMethodology mClient mId mRq'
      mCheckUpdated methodology

    it "throws a 400 for try to set not unique name" $ do
      let mRq' = mRq { mrqName = methodologyName1 }
      apiTry env (meEditMethodology mClient mId mRq') `shouldThrow`
        errorWithStatus badRequest400

    it "throws a 404 for unknown methodology" $ do
      apiTry env (meEditMethodology mClient unknownSqlId mRq) `shouldThrow`
        errorWithStatus notFound404

  describe "DELETE /methodology/{methodologyId}" $ do
    it "allows to delete methodology" $ do
      _ <- apiTry env $ meDeleteMethodology mClient mId
      apiTry env (meGetMethodology mClient mId) `shouldThrow`
        errorWithStatus notFound404

    it "throws a 404 for unknown methodology" $ do
      apiTry env (meDeleteMethodology mClient unknownSqlId) `shouldThrow`
        errorWithStatus notFound404
  where
    mClient :: RunClient m => MethodologyEndpoints (AsClientT m)
    mClient = genericClient

    mId :: MethodologyId
    mId = SqlId 2

    mRq :: MethodologyReq
    mRq = MethodologyReq
      { mrqName = "new"
      , mrqDescription = Just "description"
      , mrqConfluence = Just sampleURI
      }

    mCheck :: (MethodologyId, Text, Maybe Text, Maybe URI, [Text])
           -> Methodology
           -> IO ()
    mCheck (mi, mn, md, mc, mp) (WithId id_ response) = do
      id_ `shouldBe` mi
      mrName response `shouldBe` mn
      mrDescription response `shouldBe` md
      mrConfluence response `shouldBe` mc
      mrProjects response `shouldBe` mp

    mCheckLast, mCheckUpdated, mCheckCreated :: Methodology -> IO ()
    mCheckLast = mCheck (mId, methodologyName2, methodologyDescription2, methodologyConfluence2, [projectName2])
    mCheckUpdated = mCheck (mId, "rename", mrqDescription mRq, mrqConfluence mRq, [projectName2])
    mCheckCreated = mCheck (SqlId 3, mrqName mRq, mrqDescription mRq,  mrqConfluence mRq, [])


type Project = WithId 'ProjectId ProjectResp

projectSpec :: ClientEnv -> Spec
projectSpec env = do
  describe "GET /projects" $ do
    it "allows to list all projects" $ do
      projects@(project:_) <- apiTry env $
        peGetProjects pClient noSorting fullContent
      length projects `shouldBe` 2
      pCheckLast project

  describe "GET /project/{projectId}" $ do
    it "allows to get project" $ do
      project <- apiTry env $ peGetProject pClient pId
      pCheckLast project

    it "throws a 404 for unknown project" $ do
      apiTry env (peGetProject pClient unknownSqlId) `shouldThrow`
        errorWithStatus notFound404

  describe "POST /project" $ do
    it "allows to create project" $ do
      project <- apiTry env $ peAddProject pClient pRq
      pCheckCreated project

    it "throws a 400 for try to set not unique name" $ do
      let pRq' = pRq { prqName = projectName1 }
      apiTry env (peAddProject pClient pRq') `shouldThrow`
        errorWithStatus badRequest400

  describe "PUT /project/{projectId}" $ do
    it "allows to update project" $ do
      let pRq' = pRq { prqName = "rename" }
      project <- apiTry env $ peEditProject pClient pId pRq'
      pCheckUpdated project

    it "throws a 400 for try to set not unique name" $ do
      let pRq' = pRq { prqName = projectName1 }
      apiTry env (peEditProject pClient pId pRq') `shouldThrow`
        errorWithStatus badRequest400

    it "throws a 404 for unknown project" $ do
      apiTry env (peEditProject pClient unknownSqlId pRq) `shouldThrow`
        errorWithStatus notFound404
  where
    pClient :: RunClient m => ProjectEndpoints (AsClientT m)
    pClient = genericClient

    pId :: ProjectId
    pId = SqlId 2

    pRq :: ProjectReq
    pRq = ProjectReq
      { prqName = "new"
      , prqDescription = Just "description"
      }

    pCheck :: (ProjectId, Text, Maybe Text, [Text]) -> Project -> IO ()
    pCheck (pi', pn, pd, pc) (WithId id_ response) = do
      id_ `shouldBe` pi'
      prName response `shouldBe` pn
      prDescription response `shouldBe` pd
      prCompoundNames response `shouldBe` pc

    pCheckLast, pCheckUpdated, pCheckCreated :: Project -> IO ()
    pCheckLast = pCheck (pId, projectName2, projectDescription2, [compoundName1, compoundName2, compoundName5])
    pCheckUpdated = pCheck (pId, "rename", prqDescription pRq, [compoundName1, compoundName2, compoundName5])
    pCheckCreated = pCheck (SqlId 3, prqName pRq, prqDescription pRq, [])
