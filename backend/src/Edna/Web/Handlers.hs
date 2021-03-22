module Edna.Web.Handlers
  ( ednaHandlers
  ) where

import Universum

import Servant.API.Generic (ToServant)
import Servant.Server (err501)
import Servant.Server.Generic (AsServerT, genericServerT)

import qualified Edna.Upload.API as Upload

import Edna.Setup (Edna)
import Edna.Web.API
  (CompoundEndpoints(..), EdnaEndpoints(..), MethodologyEndpoints(..), ProjectEndpoints(..),
  TargetEndpoints(..))

type EdnaHandlers m = ToServant EdnaEndpoints (AsServerT m)

-- | Server handler implementation for Edna API.
ednaHandlers :: EdnaHandlers Edna
ednaHandlers = genericServerT EdnaEndpoints
  { eeUploadExperiment = Upload.uploadExperiment
  , eeFileUploadEndpoints = Upload.fileUploadEndpoints
  , eeProjectEndpoints = projectEndpoints
  , eeMethodologyEndpoints = methodologyEndpoints
  , eeCompoundEndpoints = compoundEndpoints
  , eeTargetEndpoints = targetEndpoints
  }
  where
    projectEndpoints = genericServerT ProjectEndpoints
      { peAddProject = \_ -> throwM err501
      , peEditProject = \_ _ -> throwM err501
      , peGetProjects = \_ _ _ -> throwM err501
      , peGetProject = \_ -> throwM err501
      }

    methodologyEndpoints = genericServerT MethodologyEndpoints
      { meAddMethodology = \_ -> throwM err501
      , meEditMethodology = \_ _ -> throwM err501
      , meDeleteMethodology = \_ -> throwM err501
      , meGetMethodologies = \_ _ _ -> throwM err501
      , meGetMethodology = \_ -> throwM err501
      }

    compoundEndpoints = genericServerT CompoundEndpoints
      { ceEditChemSoft = \_ _ -> throwM err501
      , ceGetCompounds = \_ _ _ -> throwM err501
      , ceGetCompound = \_ -> throwM err501
      }

    targetEndpoints = genericServerT TargetEndpoints
      { teGetTargets = \_ _ _ -> throwM err501
      , teGetTarget = \_ -> throwM err501
      }
