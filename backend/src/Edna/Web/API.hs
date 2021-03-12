{-|
Servant type-level specification for Edna API.
-}
module Edna.Web.API
  ( EdnaAPI
  , EdnaEndpoints (..)
  , ednaAPI
  ) where

import Universum

import Servant.API (Capture, Get, JSON, Post, Put, QueryParam, ReqBody, Summary, (:>))
import Servant.API.Generic (AsApi, ToServant, (:-))
import Servant.Multipart (Mem, MultipartData(..), MultipartForm)

import Edna.Web.Types

-- | API endpoints specification.
data EdnaEndpoints route = EdnaEndpoints
  { -- | Legacy: upload one experiment
    eeUploadExperiment :: route
      :- "experiment"
      :> Summary "Upload an EXCEL file describing one experiment"
      :> MultipartForm Mem (MultipartData Mem)
      :> Post '[JSON] [ExperimentalMeasurement]

  , -- | Add a new project.
    eeAddProject :: route
      :- "project"
      :> Summary "Add a new project"
      :> ReqBody '[JSON] Project
      :> Post '[JSON] (WithId Project)

  , -- | Update an existing project.
    eeEditProject :: route
      :- "project"
      :> Summary "Update an existing project"
      :> Capture "projectId" (SqlId Project)
      :> ReqBody '[JSON] Project
      :> Put '[JSON] (WithId Project)

  -- TODO: pagination and sorting are just stubs for now (everywhere).
  -- Most likely we will use @servant-util@ to implement them,
  -- but let's do it later.

  , -- | Get known projects with optional pagination and sorting
    eeGetProjects :: route
      :- "projects"
      :> Summary "Get known projects"
      :> QueryParam "page" Word
      :> QueryParam "size" Word
      :> QueryParam "sortby" StubSortBy
      :> Get '[JSON] [WithExtra Project ProjectExtra]

  , -- | Get project data by ID
    eeGetProject :: route
      :- "project"
      :> Summary "Get project data by ID"
      :> Capture "projectId" (SqlId Project)
      :> Get '[JSON] (WithExtra Project ProjectExtra)

  , -- | Add a new methodology.
    eeAddMethodology :: route
      :- "methodology"
      :> Summary "Add a new methodology"
      :> ReqBody '[JSON] TestMethodology
      :> Post '[JSON] (WithId TestMethodology)

  , -- | Update an existing methodology.
    eeEditMethodology :: route
      :- "methodology"
      :> Summary "Update an existing methodology"
      :> Capture "methodologyId" (SqlId TestMethodology)
      :> ReqBody '[JSON] TestMethodology
      :> Put '[JSON] (WithId TestMethodology)

  , -- | Get known methodologies with optional pagination and sorting
    eeGetMethodologies :: route
      :- "methodologies"
      :> Summary "Get known methodologies"
      :> QueryParam "page" Word
      :> QueryParam "size" Word
      :> QueryParam "sortby" StubSortBy
      :> Get '[JSON] [WithId TestMethodology]

  , -- | Get methodology data by ID
    eeGetMethodology :: route
      :- "methodology"
      :> Summary "Get methodology data by ID"
      :> Capture "methodologyId" (SqlId TestMethodology)
      :> Get '[JSON] (WithId TestMethodology)

  } deriving stock (Generic)

-- | API type specification.
type EdnaAPI =
  "api" :> ToServant EdnaEndpoints AsApi

ednaAPI :: Proxy EdnaAPI
ednaAPI = Proxy
