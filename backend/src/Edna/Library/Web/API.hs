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
import Servant.API (Capture, Delete, Get, JSON, NoContent, Post, Put, Summary, (:>))
import Servant.API.Generic (AsApi, ToServant, (:-))
import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.Util (PaginationParams, SortingParamsOf)

import Edna.Library.Service
  (addMethodology, addProject, deleteMethodology, editChemSoft, editMde, getCompound, getCompounds,
  getMethodologies, getMethodology, getProject, getProjects, getTarget, getTargets,
  updateMethodology, updateProject)
import Edna.Library.Web.Types
  (CompoundResp, MethodologyReq, MethodologyResp, ProjectReq, ProjectResp, TargetResp)
import Edna.Setup (Edna)
import Edna.Util (IdType(..), MethodologyId, SqlId(..))
import Edna.Web.Types (URI, WithId)

-- | Endpoints related to projects.
data ProjectEndpoints route = ProjectEndpoints
  { -- | Add a new project.
    peAddProject :: route
      :- "project"
      :> Summary "Add a new project"
      :> ReqBody '[JSON] ProjectReq
      :> Post '[JSON] (WithId 'ProjectId ProjectResp)

  , -- | Update an existing project.
    peEditProject :: route
      :- "project"
      :> Summary "Update an existing project"
      :> Capture "projectId" (SqlId 'ProjectId)
      :> ReqBody '[JSON] ProjectReq
      :> Put '[JSON] (WithId 'ProjectId ProjectResp)

  , -- | Get known projects with optional pagination and sorting
    peGetProjects :: route
      :- "projects"
      :> Summary "Get known projects"
      :> SortingParamsOf ProjectResp
      :> PaginationParams
      :> Get '[JSON] [WithId 'ProjectId ProjectResp]

  , -- | Get project data by ID
    peGetProject :: route
      :- "project"
      :> Summary "Get project data by ID"
      :> Capture "projectId" (SqlId 'ProjectId)
      :> Get '[JSON] (WithId 'ProjectId ProjectResp)
  } deriving stock (Generic)

type ProjectAPI = ToServant ProjectEndpoints AsApi

projectEndpoints :: ToServant ProjectEndpoints (AsServerT Edna)
projectEndpoints = genericServerT ProjectEndpoints
  { peAddProject = addProject
  , peEditProject = updateProject
  , peGetProjects = getProjects
  , peGetProject = getProject
  }

-- | Endpoints related to methodologies.
data MethodologyEndpoints route = MethodologyEndpoints
  { -- | Add a new methodology.
    meAddMethodology :: route
      :- "methodology"
      :> Summary "Add a new methodology"
      :> ReqBody '[JSON] MethodologyReq
      :> Post '[JSON] (WithId 'MethodologyId MethodologyResp)

  , -- | Update an existing methodology.
    meEditMethodology :: route
      :- "methodology"
      :> Summary "Update an existing methodology"
      :> Capture "methodologyId" MethodologyId
      :> ReqBody '[JSON] MethodologyReq
      :> Put '[JSON] (WithId 'MethodologyId MethodologyResp)

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
      :> SortingParamsOf MethodologyResp
      :> PaginationParams
      :> Get '[JSON] [WithId 'MethodologyId MethodologyResp]

  , -- | Get methodology data by ID
    meGetMethodology :: route
      :- "methodology"
      :> Summary "Get methodology data by ID"
      :> Capture "methodologyId" MethodologyId
      :> Get '[JSON] (WithId 'MethodologyId MethodologyResp)
  } deriving stock (Generic)

type MethodologyAPI = ToServant MethodologyEndpoints AsApi

methodologyEndpoints :: ToServant MethodologyEndpoints (AsServerT Edna)
methodologyEndpoints = genericServerT MethodologyEndpoints
  { meAddMethodology = addMethodology
  , meEditMethodology = updateMethodology
  , meDeleteMethodology = deleteMethodology
  , meGetMethodologies = getMethodologies
  , meGetMethodology = getMethodology
  }

-- | Endpoints related to targets.
data TargetEndpoints route = TargetEndpoints
  { -- | Get known targets with optional pagination and sorting
    teGetTargets :: route
      :- "targets"
      :> Summary "Get known targets"
      :> SortingParamsOf TargetResp
      :> PaginationParams
      :> Get '[JSON] [WithId 'TargetId TargetResp]

  , -- | Get target data by ID
    teGetTarget :: route
      :- "target"
      :> Summary "Get target data by ID"
      :> Capture "targetId" (SqlId 'TargetId)
      :> Get '[JSON] (WithId 'TargetId TargetResp)
  } deriving stock (Generic)

type TargetAPI = ToServant TargetEndpoints AsApi

targetEndpoints :: ToServant TargetEndpoints (AsServerT Edna)
targetEndpoints = genericServerT TargetEndpoints
  { teGetTargets = getTargets
  , teGetTarget = getTarget
  }

-- | Endpoints related to compounds.
data CompoundEndpoints route = CompoundEndpoints
  { -- | Edit ChemSoft link for the compound with given ID
    ceEditChemSoft :: route
      :- "compound"
      :> "chemsoft"
      :> Summary "Update ChemSoft link of a compound"
      :> Capture "compoundId" (SqlId 'CompoundId)
      :> ReqBody '[JSON] URI
      :> Put '[JSON] (WithId 'CompoundId CompoundResp)

  , -- | Edit MDe link for the compound with given ID
    ceEditMde :: route
      :- "compound"
      :> "mde"
      :> Summary "Update Mde link of a compound"
      :> Capture "compoundId" (SqlId 'CompoundId)
      :> ReqBody '[JSON] URI
      :> Put '[JSON] (WithId 'CompoundId CompoundResp)

  , -- | Get known compounds with optional pagination and sorting
    ceGetCompounds :: route
      :- "compounds"
      :> Summary "Get known compounds"
      :> SortingParamsOf CompoundResp
      :> PaginationParams
      :> Get '[JSON] [WithId 'CompoundId CompoundResp]

  , -- | Get compound data by ID
    ceGetCompound :: route
      :- "compound"
      :> Summary "Get compound data by ID"
      :> Capture "compoundId" (SqlId 'CompoundId)
      :> Get '[JSON] (WithId 'CompoundId CompoundResp)
  } deriving stock (Generic)

type CompoundAPI = ToServant CompoundEndpoints AsApi

compoundEndpoints :: ToServant CompoundEndpoints (AsServerT Edna)
compoundEndpoints = genericServerT CompoundEndpoints
  { ceEditChemSoft = editChemSoft
  , ceEditMde = editMde
  , ceGetCompounds = getCompounds
  , ceGetCompound = getCompound
  }
