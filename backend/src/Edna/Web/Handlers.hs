-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

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
  { eeFileUploadEndpoints = Upload.fileUploadEndpoints
  , eeProjectEndpoints = Library.projectEndpoints
  , eeMethodologyEndpoints = Library.methodologyEndpoints
  , eeCompoundEndpoints = Library.compoundEndpoints
  , eeTargetEndpoints = Library.targetEndpoints
  , eeDashboardEndpoints = Dashboard.dashboardEndpoints
  }
