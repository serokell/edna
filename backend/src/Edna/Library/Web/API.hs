-- | Library-related part of API definition along with implementation.

module Edna.Library.Web.API
  ( TargetEndpoints (..)
  , TargetAPI
  , targetEndpoints

  , CompoundEndpoints (..)
  , CompoundAPI
  , compoundEndpoints
  ) where

import Universum

import Servant (ReqBody)
import Servant.API ((:>), Capture, Get, JSON, Put, QueryParam, Summary)
import Servant.API.Generic ((:-), AsApi, ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Edna.Library.Service (editChemSoft, getCompound, getCompounds, getTarget, getTargets)
import Edna.Library.Web.Types (CompoundResp, TargetResp)
import Edna.Setup (Edna)
import Edna.Util (IdType(..), SqlId(..))
import Edna.Web.Types (StubSortBy, URI, WithId)

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
