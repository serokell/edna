{-|
Servant type-level specification for Edna API.
-}
module Edna.Web.API
  ( EdnaAPI
  , EdnaEndpoints (..)
  , ednaAPI

  -- * Subtypes
  , ProjectEndpoints (..)
  ) where

import Universum

import Servant.API ((:>), Capture, Get, JSON, Post, Put, QueryParam, ReqBody, Summary)
import Servant.API.Generic ((:-), AsApi, ToServant)
import Servant.Multipart (Mem, MultipartData(..), MultipartForm)

import qualified Edna.Upload.API as Upload

import Edna.Library.Web.API (CompoundAPI, MethodologyAPI, TargetAPI)
import Edna.Util (SqlId(..))
import Edna.Web.Types

-- | API endpoints specification.
data EdnaEndpoints route = EdnaEndpoints
  { -- | Legacy: upload one experiment
    eeUploadExperiment :: route
      :- "experiment"
      :> Summary "Upload an EXCEL file describing one experiment"
      :> MultipartForm Mem (MultipartData Mem)
      :> Post '[JSON] [Upload.ExperimentalMeasurement]

  , eeFileUploadEndpoints :: route :- "file" :> Upload.FileUploadAPI
  , eeProjectEndpoints :: route :- ProjectAPI
  , eeMethodologyEndpoints :: route :- MethodologyAPI
  , eeCompoundEndpoints :: route :- CompoundAPI
  , eeTargetEndpoints :: route :- TargetAPI
  } deriving stock (Generic)

-- | API type specification.
type EdnaAPI =
  "api" :> ToServant EdnaEndpoints AsApi

ednaAPI :: Proxy EdnaAPI
ednaAPI = Proxy

-- TODO: pagination and sorting are just stubs for now (everywhere).
-- Most likely we will use @servant-util@ to implement them,
-- but let's do it later.

-- | Endpoints related to projects.
data ProjectEndpoints route = ProjectEndpoints
  { -- | Add a new project.
    peAddProject :: route
      :- "project"
      :> Summary "Add a new project"
      :> ReqBody '[JSON] Project
      :> Post '[JSON] (WithExtra Project Project ProjectExtra)

  , -- | Update an existing project.
    peEditProject :: route
      :- "project"
      :> Summary "Update an existing project"
      :> Capture "projectId" (SqlId Project)
      :> ReqBody '[JSON] Project
      :> Put '[JSON] (WithExtra Project Project ProjectExtra)

  , -- | Get known projects with optional pagination and sorting
    peGetProjects :: route
      :- "projects"
      :> Summary "Get known projects"
      :> QueryParam "page" Word
      :> QueryParam "size" Word
      :> QueryParam "sortby" StubSortBy
      :> Get '[JSON] [WithExtra Project Project ProjectExtra]

  , -- | Get project data by ID
    peGetProject :: route
      :- "project"
      :> Summary "Get project data by ID"
      :> Capture "projectId" (SqlId Project)
      :> Get '[JSON] (WithExtra Project Project ProjectExtra)
  } deriving stock (Generic)

type ProjectAPI = ToServant ProjectEndpoints AsApi
