-- | Library-related part of API definition along with implementation.

module Edna.Library.Web.API
  ( TargetEndpoints (..)
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
import Servant.API ((:>), Capture, Delete, Get, JSON, NoContent, Post, Put, QueryParam, Summary)
import Servant.API.Generic ((:-), AsApi, ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Edna.Library.Service
  (addMethodology, deleteMethodology, editChemSoft, getCompound, getCompounds, getMethodologies,
  getMethodology, getTarget, getTargets, updateMethodology)
import Edna.Library.Web.Types (CompoundResp, MethodologyReqResp, TargetResp)
import Edna.Setup (Edna)
import Edna.Util (IdType(..), SqlId(..))
import Edna.Web.Types (StubSortBy, URI, WithId)

-- | Endpoints related to methodologies.
data MethodologyEndpoints route = MethodologyEndpoints
  { -- | Add a new methodology.
    meAddMethodology :: route
      :- "methodology"
      :> Summary "Add a new methodology"
      :> ReqBody '[JSON] MethodologyReqResp
      :> Post '[JSON] (WithId 'MethodologyId MethodologyReqResp)

  , -- | Update an existing methodology.
    meEditMethodology :: route
      :- "methodology"
      :> Summary "Update an existing methodology"
      :> Capture "methodologyId" (SqlId 'MethodologyId)
      :> ReqBody '[JSON] MethodologyReqResp
      :> Put '[JSON] (WithId 'MethodologyId MethodologyReqResp)

  , -- | Delete an existing methodology.
    meDeleteMethodology :: route
      :- "methodology"
      :> Summary "Delete an existing methodology"
      :> Capture "methodologyId" (SqlId 'MethodologyId)
      :> Delete '[JSON] NoContent

  , -- | Get known methodologies with optional pagination and sorting
    meGetMethodologies :: route
      :- "methodologies"
      :> Summary "Get known methodologies"
      :> QueryParam "page" Word
      :> QueryParam "size" Word
      :> QueryParam "sortby" StubSortBy
      :> Get '[JSON] [WithId 'MethodologyId MethodologyReqResp]

  , -- | Get methodology data by ID
    meGetMethodology :: route
      :- "methodology"
      :> Summary "Get methodology data by ID"
      :> Capture "methodologyId" (SqlId 'MethodologyId)
      :> Get '[JSON] (WithId 'MethodologyId MethodologyReqResp)
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
      :> QueryParam "page" Word
      :> QueryParam "size" Word
      :> QueryParam "sortby" StubSortBy
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

  , -- | Get known compounds with optional pagination and sorting
    ceGetCompounds :: route
      :- "compounds"
      :> Summary "Get known compounds"
      :> QueryParam "page" Word
      :> QueryParam "size" Word
      :> QueryParam "sortby" StubSortBy
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
  , ceGetCompounds = getCompounds
  , ceGetCompound = getCompound
  }
