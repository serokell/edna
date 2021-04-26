-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

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
import Servant.Multipart (Mem, MultipartForm)

import qualified Edna.Dashboard.Web.API as Dashboard
import qualified Edna.Upload.Web.API as Upload

import Edna.Library.Web.API (CompoundAPI, MethodologyAPI, ProjectAPI, TargetAPI)
import Edna.Upload.Web.Types (FileBS)

-- | API endpoints specification.
data EdnaEndpoints route = EdnaEndpoints
  { -- | Legacy: upload one experiment
    eeUploadExperiment :: route
      :- "experiment"
      :> Summary "Upload an EXCEL file describing one experiment"
      :> MultipartForm Mem FileBS
      :> Post '[JSON] [Upload.ExperimentalMeasurement]

  , eeFileUploadEndpoints :: route :- "file" :> Upload.FileUploadAPI
  , eeProjectEndpoints :: route :- ProjectAPI
  , eeMethodologyEndpoints :: route :- MethodologyAPI
  , eeCompoundEndpoints :: route :- CompoundAPI
  , eeTargetEndpoints :: route :- TargetAPI
  , eeDashboardEndpoints :: route :- Dashboard.DashboardAPI
  } deriving stock (Generic)

-- | API type specification.
type EdnaAPI =
  ToServant EdnaEndpoints AsApi
  :<|>
  "health" :> Summary "Check the health of this server" :> GetNoContent

ednaAPI :: Proxy EdnaAPI
ednaAPI = Proxy
