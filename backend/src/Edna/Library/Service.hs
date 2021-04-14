-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

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
  (CompoundResp(..), MethodologyReq(..), MethodologyResp(..), ProjectReq(..), ProjectResp,
  TargetResp)
import Edna.Logging (logMessage)
import Edna.Setup (Edna)
import Edna.Util (IdType(..), SqlId(..), ensureOrThrow, justOrThrow, localToUTC, nothingOrThrow)
import Edna.Util.URI (parseURI, renderURI)
import Edna.Web.Types (URI, WithId(..))

-- | Get target with given ID.
getTarget :: SqlId 'TargetId -> Edna (WithId 'TargetId TargetResp)
getTarget targetSqlId =
  Q.getTargetById targetSqlId >>= justOrThrow (LETargetNotFound targetSqlId)

-- | Get all known targets.
getTargets :: Edna [WithId 'TargetId TargetResp]
getTargets = Q.getTargets

compoundToResp :: CompoundRec -> Edna (WithId 'CompoundId CompoundResp)
compoundToResp CompoundRec{..} = do
  url <- case cChemsoftLink of
    Just link -> Just <$> justOrThrow (LEInvalidURI link) (parseURI link)
    Nothing -> pure Nothing
  pure $ WithId (SqlId $ unSerial cCompoundId) $ CompoundResp
    { crName = cName
    , crChemSoft = url
    , crAdditionDate = localToUTC cAdditionDate
    }

getCompound :: SqlId 'CompoundId -> Edna (WithId 'CompoundId CompoundResp)
getCompound compoundSqlId = Q.getCompoundById compoundSqlId >>=
  justOrThrow (LECompoundNotFound compoundSqlId) >>= compoundToResp

getCompounds :: Edna [WithId 'CompoundId CompoundResp]
getCompounds = Q.getCompounds >>= mapM compoundToResp

editChemSoft :: SqlId 'CompoundId -> URI -> Edna (WithId 'CompoundId CompoundResp)
editChemSoft compoundSqlId uri = do
  compound <- Q.getCompoundById compoundSqlId >>= justOrThrow (LECompoundNotFound compoundSqlId)
  let uriText = renderURI uri
  Q.editCompoundChemSoft compoundSqlId uriText
  compoundToResp compound {cChemsoftLink = Just uriText}

methodologyToResp :: (TestMethodologyRec, [Text]) -> Edna (WithId 'MethodologyId MethodologyResp)
methodologyToResp (TestMethodologyRec{..}, projects) = do
  url <- case tmConfluenceLink of
    Just link -> Just <$> justOrThrow (LEInvalidURI link) (parseURI link)
    Nothing -> pure Nothing
  pure $ WithId (SqlId $ unSerial tmTestMethodologyId) $ MethodologyResp
    { mrName = tmName
    , mrDescription = tmDescription
    , mrConfluence = url
    , mrProjects = projects
    }

getMethodology :: SqlId 'MethodologyId -> Edna (WithId 'MethodologyId MethodologyResp)
getMethodology methodologySqlId = Q.getMethodologyById methodologySqlId >>=
  justOrThrow (LEMethodologyNotFound methodologySqlId) >>= methodologyToResp

getMethodologies
  :: Edna [WithId 'MethodologyId MethodologyResp]
getMethodologies = Q.getMethodologies >>= mapM methodologyToResp

addMethodology :: MethodologyReq -> Edna (WithId 'MethodologyId MethodologyResp)
addMethodology tm@MethodologyReq{..} = do
  Q.getMethodologyByName mrqName >>= nothingOrThrow (LEMethodologyNameExists mrqName)
  res <- Q.insertMethodology tm >>= methodologyToResp . (,[])
  res <$ logMessage ("Added methodology with name " <> mrqName)

updateMethodology
  :: SqlId 'MethodologyId
  -> MethodologyReq
  -> Edna (WithId 'MethodologyId MethodologyResp)
updateMethodology mId@(SqlId methodologyId) tm@MethodologyReq{..} = do
  existingMethodology <- Q.getMethodologyByName mrqName
  case existingMethodology of
    Just tmRec -> ensureOrThrow (LEMethodologyNameExists mrqName) $
      tmTestMethodologyId tmRec == SqlSerial methodologyId
    Nothing -> pure ()
  Q.updateMethodology mId tm >> getMethodology mId

deleteMethodology :: SqlId 'MethodologyId -> Edna NoContent
deleteMethodology methodologySqlId =
  NoContent <$ unlessM (Q.deleteMethodology methodologySqlId)
  (throwM $ LEMethodologyNotFound methodologySqlId)

getProject :: SqlId 'ProjectId -> Edna (WithId 'ProjectId ProjectResp)
getProject projectSqlId =
  Q.getProjectWithCompoundsById projectSqlId >>= justOrThrow (LEProjectNotFound projectSqlId)

getProjects :: Edna [WithId 'ProjectId ProjectResp]
getProjects = Q.getProjectsWithCompounds

addProject :: ProjectReq -> Edna (WithId 'ProjectId ProjectResp)
addProject p@ProjectReq{..} = do
  Q.getProjectByName prqName >>= nothingOrThrow (LEProjectNameExists prqName)
  ProjectRec{..} <- Q.insertProject p
  logMessage $ "Added project with name " <> prqName
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
