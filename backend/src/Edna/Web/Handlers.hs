module Edna.Web.Handlers
  ( ednaHandlers
  ) where

import Universum

import Servant.API.Generic (ToServant)
import Servant.Server (err501)
import Servant.Server.Generic (AsServerT, genericServerT)

import qualified Edna.Library.Web.API as Library
import qualified Edna.Upload.API as Upload

import Edna.Setup (Edna)
import Edna.Web.API (EdnaEndpoints(..), ProjectEndpoints(..))

type EdnaHandlers m = ToServant EdnaEndpoints (AsServerT m)

-- | Server handler implementation for Edna API.
ednaHandlers :: EdnaHandlers Edna
ednaHandlers = genericServerT EdnaEndpoints
  { eeUploadExperiment = Upload.uploadExperiment
  , eeFileUploadEndpoints = Upload.fileUploadEndpoints
  , eeProjectEndpoints = projectEndpoints
  , eeMethodologyEndpoints = Library.methodologyEndpoints
  , eeCompoundEndpoints = Library.compoundEndpoints
  , eeTargetEndpoints = Library.targetEndpoints
  }
  where
    projectEndpoints = genericServerT ProjectEndpoints
      { peAddProject = \_ -> throwM err501
      , peEditProject = \_ _ -> throwM err501
      , peGetProjects = \_ _ _ -> throwM err501
      , peGetProject = \_ -> throwM err501
      }
