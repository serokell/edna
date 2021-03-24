-- | Implementation of library functionality

module Edna.Library.Service
  ( -- * Implementation
    getTarget
  , getTargets
  , getCompound
  , getCompounds
  , editChemSoft
  , getMethodology
  , getMethodologies
  , deleteMethodology
  , addMethodology
  , updateMethodology
  , getProject
  , getProjects
  , addProject
  , updateProject
  ) where

import Universum

import qualified Edna.Library.DB.Query as Q

import Database.Beam.Backend (SqlSerial(..))
import Servant.API (NoContent(..))

import Edna.Library.DB.Schema
  (CompoundRec, CompoundT(..), ProjectT(..), TestMethodologyRec, TestMethodologyT(..))
import Edna.Library.Error (LibraryError(..))
import Edna.Library.Web.Types
  (CompoundResp(..), MethodologyReqResp(..), ProjectReq(..), ProjectResp, TargetResp)
import Edna.Setup (Edna)
import Edna.Util (IdType(..), SqlId(..), ensureOrThrow, justOrThrow, nothingOrThrow)
import Edna.Util.URI (parseURI, renderURI)
import Edna.Web.Types (StubSortBy, URI, WithId(..))

-- | Get target with given ID.
getTarget :: SqlId 'TargetId -> Edna (WithId 'TargetId TargetResp)
getTarget targetSqlId =
  Q.getTargetById targetSqlId >>= justOrThrow (LETargetNotFound targetSqlId)

-- | Get all targets with optional pagination and sorting.
-- Pagination and sorting parameters are currently ignored.
getTargets :: Maybe Word -> Maybe Word -> Maybe StubSortBy -> Edna [WithId 'TargetId TargetResp]
getTargets _ _ _ = Q.getTargets

compoundToResp :: CompoundRec -> Edna (WithId 'CompoundId CompoundResp)
compoundToResp CompoundRec{..} = do
  url <- case cChemsoftLink of
    Just link -> Just <$> justOrThrow (LEInvalidURI link) (parseURI link)
    Nothing -> pure Nothing
  pure $ WithId (SqlId $ unSerial cCompoundId) $ CompoundResp
    { crName = cName
    , crChemSoft = url
    , crAdditionDate = cAdditionDate
    }

getCompound :: SqlId 'CompoundId -> Edna (WithId 'CompoundId CompoundResp)
getCompound compoundSqlId = Q.getCompoundById compoundSqlId >>=
  justOrThrow (LECompoundNotFound compoundSqlId) >>= compoundToResp

getCompounds
  :: Maybe Word
  -> Maybe Word
  -> Maybe StubSortBy
  -> Edna [WithId 'CompoundId CompoundResp]
getCompounds _ _ _ = Q.getCompounds >>= mapM compoundToResp

editChemSoft :: SqlId 'CompoundId -> URI -> Edna (WithId 'CompoundId CompoundResp)
editChemSoft compoundSqlId uri = do
  compound <- Q.getCompoundById compoundSqlId >>= justOrThrow (LECompoundNotFound compoundSqlId)
  let uriText = renderURI uri
  Q.editCompoundChemSoft compoundSqlId uriText
  compoundToResp compound {cChemsoftLink = Just uriText}

methodologyToResp :: TestMethodologyRec -> Edna (WithId 'MethodologyId MethodologyReqResp)
methodologyToResp TestMethodologyRec{..} = do
  url <- case tmConfluenceLink of
    Just link -> Just <$> justOrThrow (LEInvalidURI link) (parseURI link)
    Nothing -> pure Nothing
  pure $ WithId (SqlId $ unSerial tmTestMethodologyId) $ MethodologyReqResp
    { mrpName = tmName
    , mrpDescription = tmDescription
    , mrpConfluence = url
    }

getMethodology :: SqlId 'MethodologyId -> Edna (WithId 'MethodologyId MethodologyReqResp)
getMethodology methodologySqlId = Q.getMethodologyById methodologySqlId >>=
  justOrThrow (LEMethodologyNotFound methodologySqlId) >>= methodologyToResp

getMethodologies
  :: Maybe Word
  -> Maybe Word
  -> Maybe StubSortBy
  -> Edna [WithId 'MethodologyId MethodologyReqResp]
getMethodologies _ _ _ = Q.getMethodologies >>= mapM methodologyToResp

addMethodology :: MethodologyReqResp -> Edna (WithId 'MethodologyId MethodologyReqResp)
addMethodology tm@MethodologyReqResp{..} = do
  Q.getMethodologyByName mrpName >>= nothingOrThrow (LEMethodologyNameExists mrpName)
  Q.insertMethodology tm >>= methodologyToResp

updateMethodology
  :: SqlId 'MethodologyId
  -> MethodologyReqResp
  -> Edna (WithId 'MethodologyId MethodologyReqResp)
updateMethodology mId@(SqlId methodologyId) tm@MethodologyReqResp{..} = do
  existingMethodology <- Q.getMethodologyByName mrpName
  case existingMethodology of
    Just tmRec -> ensureOrThrow (LEMethodologyNameExists mrpName) $
      tmTestMethodologyId tmRec == SqlSerial methodologyId
    Nothing -> pure ()
  Q.updateMethodology mId tm >> getMethodology mId

deleteMethodology :: SqlId 'MethodologyId -> Edna NoContent
deleteMethodology methodologySqlId =
  Q.deleteMethodology methodologySqlId >> pure NoContent

getProject :: SqlId 'ProjectId -> Edna (WithId 'ProjectId ProjectResp)
getProject projectSqlId =
  Q.getProjectWithCompoundsById projectSqlId >>= justOrThrow (LEProjectNotFound projectSqlId)

getProjects :: Maybe Word -> Maybe Word -> Maybe StubSortBy -> Edna [WithId 'ProjectId ProjectResp]
getProjects _ _ _ = Q.getProjectsWithCompounds

addProject :: ProjectReq -> Edna (WithId 'ProjectId ProjectResp)
addProject p@ProjectReq{..} = do
  Q.getProjectByName prqName >>= nothingOrThrow (LEProjectNameExists prqName)
  ProjectRec{..} <- Q.insertProject p
  getProject $ SqlId $ unSerial pProjectId

updateProject
  :: SqlId 'ProjectId
  -> ProjectReq
  -> Edna (WithId 'ProjectId ProjectResp)
updateProject pId@(SqlId projectId) p@ProjectReq{..} = do
  existingProject <- Q.getProjectByName prqName
  case existingProject of
    Just pRec -> ensureOrThrow (LEProjectNameExists prqName) $
      pProjectId pRec == SqlSerial projectId
    Nothing -> pure ()
  Q.updateProject pId p >> getProject pId
