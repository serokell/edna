-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Library-related part of API definition along with implementation.

module Edna.Library.Web.API
  ( ProjectEndpoints (..)
  , ProjectAPI
  , projectEndpoints

  , TargetEndpoints (..)
  , TargetAPI
  , targetEndpoints

  , CompoundEndpoints (..)
  , CompoundAPI
  , compoundEndpoints

  , MethodologyEndpoints (..)
  , MethodologyAPI
  , methodologyEndpoints
  ) where

import Universum

import Servant (ReqBody)
import Servant.API
  (Capture, Delete, Get, Header, Headers, JSON, NoContent, Post, Put, Summary, (:>))
import Servant.API.Generic (AsApi, ToServant, (:-))
import Servant.Pagination (PageHeaders, Ranges, applyRange, extractRange, returnRange)
import Servant.Server.Generic (AsServerT, genericServerT)

import Edna.Library.Service
  (addMethodology, addProject, deleteMethodology, editChemSoft, getCompound, getCompounds,
  getMethodologies, getMethodology, getProject, getProjects, getTarget, getTargets,
  updateMethodology, updateProject)
import Edna.Library.Web.Types
  (CompoundPaginationFields, CompoundWithId, MethodologyPaginationFields, MethodologyReq,
  MethodologyWithId, ProjectPaginationFields, ProjectReq, ProjectWithId, TargetPaginationFields,
  TargetWithId)
import Edna.Setup (Edna)
import Edna.Util (CompoundId, MethodologyId, ProjectId, TargetId)
import Edna.Web.Pagination (getPaginatedHelper, moreThanOnePagination, noPageHeaders)
import Edna.Web.Types (URI)

-- | Endpoints related to projects.
data ProjectEndpoints route = ProjectEndpoints
  { -- | Add a new project.
    peAddProject :: route
      :- "project"
      :> Summary "Add a new project"
      :> ReqBody '[JSON] ProjectReq
      :> Post '[JSON] ProjectWithId

  , -- | Update an existing project.
    peEditProject :: route
      :- "project"
      :> Summary "Update an existing project"
      :> Capture "projectId" ProjectId
      :> ReqBody '[JSON] ProjectReq
      :> Put '[JSON] ProjectWithId

  , -- | Get known projects with optional pagination and sorting
    peGetProjects :: route
      :- "projects"
      :> Summary "Get known projects"
      :> Header "Range" (Ranges ProjectPaginationFields ProjectWithId)
      :> Get '[JSON] (Headers ProjectsHeaders [ProjectWithId])

  , -- | Get project data by ID
    peGetProject :: route
      :- "project"
      :> Summary "Get project data by ID"
      :> Capture "projectId" ProjectId
      :> Get '[JSON] ProjectWithId
  } deriving stock (Generic)

type ProjectsHeaders = PageHeaders ProjectPaginationFields ProjectWithId

type ProjectAPI = ToServant ProjectEndpoints AsApi

projectEndpoints :: ToServant ProjectEndpoints (AsServerT Edna)
projectEndpoints = genericServerT ProjectEndpoints
  { peAddProject = addProject
  , peEditProject = updateProject
  , peGetProjects =
    \mRanges -> getPaginatedHelper mRanges getProjects $ \ranges allProjects -> do
      let nameRange = extractRange @_ @"name" ranges
      let creationDateRange = extractRange @_ @"creationDate" ranges
      let lastUpdateRange = extractRange @_ @"lastUpdate" ranges
      case (nameRange, creationDateRange, lastUpdateRange) of
        (Just r, Nothing, Nothing) -> returnRange r $ applyRange r allProjects
        (Nothing, Just r, Nothing) -> returnRange r $ applyRange r allProjects
        (Nothing, Nothing, Just r) -> returnRange r $ applyRange r allProjects
        (Nothing, Nothing, Nothing) -> return $ noPageHeaders allProjects
        _ -> throwM moreThanOnePagination
  , peGetProject = getProject
  }

-- | Endpoints related to methodologies.
data MethodologyEndpoints route = MethodologyEndpoints
  { -- | Add a new methodology.
    meAddMethodology :: route
      :- "methodology"
      :> Summary "Add a new methodology"
      :> ReqBody '[JSON] MethodologyReq
      :> Post '[JSON] MethodologyWithId

  , -- | Update an existing methodology.
    meEditMethodology :: route
      :- "methodology"
      :> Summary "Update an existing methodology"
      :> Capture "methodologyId" MethodologyId
      :> ReqBody '[JSON] MethodologyReq
      :> Put '[JSON] MethodologyWithId

  , -- | Delete an existing methodology.
    meDeleteMethodology :: route
      :- "methodology"
      :> Summary "Delete an existing methodology"
      :> Capture "methodologyId" MethodologyId
      :> Delete '[JSON] NoContent

  , -- | Get known methodologies with optional pagination and sorting
    meGetMethodologies :: route
      :- "methodologies"
      :> Summary "Get known methodologies"
      :> Header "Range" (Ranges MethodologyPaginationFields MethodologyWithId)
      :> Get '[JSON] (Headers MethodologyHeaders [MethodologyWithId])

  , -- | Get methodology data by ID
    meGetMethodology :: route
      :- "methodology"
      :> Summary "Get methodology data by ID"
      :> Capture "methodologyId" MethodologyId
      :> Get '[JSON] MethodologyWithId
  } deriving stock (Generic)

type MethodologyHeaders = PageHeaders MethodologyPaginationFields MethodologyWithId

type MethodologyAPI = ToServant MethodologyEndpoints AsApi

methodologyEndpoints :: ToServant MethodologyEndpoints (AsServerT Edna)
methodologyEndpoints = genericServerT MethodologyEndpoints
  { meAddMethodology = addMethodology
  , meEditMethodology = updateMethodology
  , meDeleteMethodology = deleteMethodology
  , meGetMethodologies =
    \mRanges -> getPaginatedHelper mRanges getMethodologies $ \ranges allMethodologies -> do
      let nameRange = extractRange @_ @"name" ranges
      case nameRange of
        Just r -> returnRange r $ applyRange r allMethodologies
        Nothing -> return $ noPageHeaders allMethodologies
  , meGetMethodology = getMethodology
  }

-- | Endpoints related to targets.
data TargetEndpoints route = TargetEndpoints
  { -- | Get known targets with optional pagination and sorting
    teGetTargets :: route
      :- "targets"
      :> Summary "Get known targets"
      :> Header "Range" (Ranges TargetPaginationFields TargetWithId)
      :> Get '[JSON] (Headers TargetHeaders [TargetWithId])

  , -- | Get target data by ID
    teGetTarget :: route
      :- "target"
      :> Summary "Get target data by ID"
      :> Capture "targetId" TargetId
      :> Get '[JSON] TargetWithId
  } deriving stock (Generic)

type TargetHeaders = PageHeaders TargetPaginationFields TargetWithId

type TargetAPI = ToServant TargetEndpoints AsApi

targetEndpoints :: ToServant TargetEndpoints (AsServerT Edna)
targetEndpoints = genericServerT TargetEndpoints
  { teGetTargets =
    \mRanges -> getPaginatedHelper mRanges getTargets $ \ranges allTargets -> do
      let nameRange = extractRange @_ @"name" ranges
      let additionDateRange = extractRange @_ @"additionDate" ranges
      case (nameRange, additionDateRange) of
        (Just r, Nothing) -> returnRange r $ applyRange r allTargets
        (Nothing, Just r) -> returnRange r $ applyRange r allTargets
        (Nothing, Nothing) -> return $ noPageHeaders allTargets
        _ -> throwM moreThanOnePagination
  , teGetTarget = getTarget
  }

-- | Endpoints related to compounds.
data CompoundEndpoints route = CompoundEndpoints
  { -- | Edit ChemSoft link for the compound with given ID
    ceEditChemSoft :: route
      :- "compound"
      :> "chemsoft"
      :> Summary "Update ChemSoft link of a compound"
      :> Capture "compoundId" CompoundId
      :> ReqBody '[JSON] URI
      :> Put '[JSON] CompoundWithId

  , -- | Get known compounds with optional pagination and sorting
    ceGetCompounds :: route
      :- "compounds"
      :> Summary "Get known compounds"
      :> Header "Range" (Ranges CompoundPaginationFields CompoundWithId)
      :> Get '[JSON] (Headers CompoundHeaders [CompoundWithId])

  , -- | Get compound data by ID
    ceGetCompound :: route
      :- "compound"
      :> Summary "Get compound data by ID"
      :> Capture "compoundId" CompoundId
      :> Get '[JSON] CompoundWithId
  } deriving stock (Generic)

type CompoundHeaders = PageHeaders CompoundPaginationFields CompoundWithId

type CompoundAPI = ToServant CompoundEndpoints AsApi

compoundEndpoints :: ToServant CompoundEndpoints (AsServerT Edna)
compoundEndpoints = genericServerT CompoundEndpoints
  { ceEditChemSoft = editChemSoft
  , ceGetCompounds =
    \mRanges -> getPaginatedHelper mRanges getCompounds $ \ranges allCompounds -> do
      let nameRange = extractRange @_ @"name" ranges
      let additionDateRange = extractRange @_ @"additionDate" ranges
      case (nameRange, additionDateRange) of
        (Just r, Nothing) -> returnRange r $ applyRange r allCompounds
        (Nothing, Just r) -> returnRange r $ applyRange r allCompounds
        (Nothing, Nothing) -> return $ noPageHeaders allCompounds
        _ -> throwM moreThanOnePagination
  , ceGetCompound = getCompound
  }
