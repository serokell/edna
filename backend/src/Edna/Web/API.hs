{-|
Servant type-level specification for Edna API.
-}
module Edna.Web.API
  ( EdnaAPI
  , EdnaEndpoints (..)
  , ednaAPI
  ) where

import Universum

import Servant.API (GetNoContent, JSON, Post, Summary, (:<|>), (:>))
import Servant.API.Generic (AsApi, ToServant, (:-))
import Servant.Multipart (Mem, MultipartData(..), MultipartForm)

import qualified Edna.Upload.API as Upload

import Edna.Library.Web.API (CompoundAPI, MethodologyAPI, ProjectAPI, TargetAPI)

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
  ToServant EdnaEndpoints AsApi
  :<|>
  "health" :> Summary "Check the health of this server" :> GetNoContent

ednaAPI :: Proxy EdnaAPI
ednaAPI = Proxy
