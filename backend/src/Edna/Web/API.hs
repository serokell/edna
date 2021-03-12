{-|
Servant type-level specification for Edna API.
-}
module Edna.Web.API
  ( EdnaAPI
  , EdnaEndpoints (..)
  , ednaAPI

  -- * Subtypes
  , FileUploadEndpoints (..)
  , ProjectEndpoints (..)
  , MethodologyEndpoints (..)
  , CompoundEndpoints (..)
  , TargetEndpoints (..)
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

  , eeFileUploadEndpoints :: route :- "file" :> FileUploadAPI
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

-- | Endpoints necessary to implement file uploading.
data FileUploadEndpoints route = FileUploadEndpoints
  { -- | Parse the file and return its summary for preview.
    fueParseFile :: route
      :- "parse"
      :> Summary "Parse the file and return its summary for preview"
      :> MultipartForm Mem (MultipartData Mem)
      :> Get '[JSON] FileSummary

  , -- | Upload the file with some methodology and project.
    fueUploadFile :: route
      :- "upload"
      :> Summary "Upload the file with some methodology and project"
      :> ReqBody '[JSON] FileUploadReq
      :> MultipartForm Mem (MultipartData Mem)
      :> Post '[JSON] FileSummary
  } deriving stock (Generic)

type FileUploadAPI = ToServant FileUploadEndpoints AsApi

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
      :> Post '[JSON] (WithId Project)

  , -- | Update an existing project.
    peEditProject :: route
      :- "project"
      :> Summary "Update an existing project"
      :> Capture "projectId" (SqlId Project)
      :> ReqBody '[JSON] Project
      :> Put '[JSON] (WithId Project)

  , -- | Get known projects with optional pagination and sorting
    peGetProjects :: route
      :- "projects"
      :> Summary "Get known projects"
      :> QueryParam "page" Word
      :> QueryParam "size" Word
      :> QueryParam "sortby" StubSortBy
      :> Get '[JSON] [WithExtra Project ProjectExtra]

  , -- | Get project data by ID
    peGetProject :: route
      :- "project"
      :> Summary "Get project data by ID"
      :> Capture "projectId" (SqlId Project)
      :> Get '[JSON] (WithExtra Project ProjectExtra)
  } deriving stock (Generic)

type ProjectAPI = ToServant ProjectEndpoints AsApi

-- | Endpoints related to methodologies.
data MethodologyEndpoints route = MethodologyEndpoints
  { -- | Add a new methodology.
    meAddMethodology :: route
      :- "methodology"
      :> Summary "Add a new methodology"
      :> ReqBody '[JSON] TestMethodology
      :> Post '[JSON] (WithId TestMethodology)

  , -- | Update an existing methodology.
    meEditMethodology :: route
      :- "methodology"
      :> Summary "Update an existing methodology"
      :> Capture "methodologyId" (SqlId TestMethodology)
      :> ReqBody '[JSON] TestMethodology
      :> Put '[JSON] (WithId TestMethodology)

  , -- | Get known methodologies with optional pagination and sorting
    meGetMethodologies :: route
      :- "methodologies"
      :> Summary "Get known methodologies"
      :> QueryParam "page" Word
      :> QueryParam "size" Word
      :> QueryParam "sortby" StubSortBy
      :> Get '[JSON] [WithId TestMethodology]

  , -- | Get methodology data by ID
    meGetMethodology :: route
      :- "methodology"
      :> Summary "Get methodology data by ID"
      :> Capture "methodologyId" (SqlId TestMethodology)
      :> Get '[JSON] (WithId TestMethodology)
  } deriving stock (Generic)

type MethodologyAPI = ToServant MethodologyEndpoints AsApi

-- | Endpoints related to compounds.
data CompoundEndpoints route = CompoundEndpoints
  { -- | Edit ChemSoft link for the compound with given ID
    ceEditChemSoft :: route
      :- "compound"
      :> "chemsoft"
      :> Summary "Update ChemSoft link of a compound"
      :> Capture "compoundId" (SqlId Compound)
      :> ReqBody '[JSON] URI
      :> Put '[JSON] (WithId Compound)

  , -- | Get known compounds with optional pagination and sorting
    ceGetCompounds :: route
      :- "compounds"
      :> Summary "Get known compounds"
      :> QueryParam "page" Word
      :> QueryParam "size" Word
      :> QueryParam "sortby" StubSortBy
      :> Get '[JSON] [WithId Compound]

  , -- | Get compound data by ID
    ceGetCompound :: route
      :- "compound"
      :> Summary "Get compound data by ID"
      :> Capture "compoundId" (SqlId Compound)
      :> Get '[JSON] (WithId Compound)
  } deriving stock (Generic)

type CompoundAPI = ToServant CompoundEndpoints AsApi

-- | Endpoints related to targets.
data TargetEndpoints route = TargetEndpoints
  { -- | Get known targets with optional pagination and sorting
    teGetTargets :: route
      :- "targets"
      :> Summary "Get known targets"
      :> QueryParam "page" Word
      :> QueryParam "size" Word
      :> QueryParam "sortby" StubSortBy
      :> Get '[JSON] [WithId Target]

  , -- | Get target data by ID
    teGetTarget :: route
      :- "target"
      :> Summary "Get target data by ID"
      :> Capture "targetId" (SqlId Target)
      :> Get '[JSON] (WithId Target)
  } deriving stock (Generic)

type TargetAPI = ToServant TargetEndpoints AsApi
