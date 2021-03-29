module Edna.Web.Handlers
  ( ednaHandlers
  ) where

import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServerT, genericServerT)

import qualified Edna.Dashboard.Web.API as Dashboard
import qualified Edna.Library.Web.API as Library
import qualified Edna.Upload.Web.API as Upload

import Edna.Setup (Edna)
import Edna.Web.API (EdnaEndpoints(..))

type EdnaHandlers m = ToServant EdnaEndpoints (AsServerT m)

-- | Server handler implementation for Edna API.
ednaHandlers :: EdnaHandlers Edna
ednaHandlers = genericServerT EdnaEndpoints
  { eeUploadExperiment = Upload.uploadExperiment
  , eeFileUploadEndpoints = Upload.fileUploadEndpoints
  , eeProjectEndpoints = Library.projectEndpoints
  , eeMethodologyEndpoints = Library.methodologyEndpoints
  , eeCompoundEndpoints = Library.compoundEndpoints
  , eeTargetEndpoints = Library.targetEndpoints
  , eeDashboardEndpoints = Dashboard.dashboardEndpoints
  }
