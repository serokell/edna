-- | Library-related part of API definition along with implementation.

module Edna.Library.Web.API
  ( TargetEndpoints (..)
  , TargetAPI
  , targetEndpoints
  ) where

import Universum

import Servant.API ((:>), Capture, Get, JSON, QueryParam, Summary)
import Servant.API.Generic ((:-), AsApi, ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import Edna.Library.Service (getTarget, getTargets)
import Edna.Library.Web.Types (TargetResp)
import Edna.Setup (Edna)
import Edna.Util (IdType(..), SqlId(..))
import Edna.Web.Types (StubSortBy, WithId)

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
