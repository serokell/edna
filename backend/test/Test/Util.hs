-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Utilities used only in tests.

module Test.Util
  ( methodologyReqToResp
  ) where

import Universum

import Edna.Library.Web.Types (MethodologyReq(..), MethodologyResp(..))

-- | Created 'MethodologyResp' from 'MethodologyReq' with a list of project names.
methodologyReqToResp :: MethodologyReq -> [Text] -> MethodologyResp
methodologyReqToResp MethodologyReq {..} projects = MethodologyResp
  { mrName = mrqName
  , mrDescription = mrqDescription
  , mrConfluence = mrqConfluence
  , mrProjects = projects
  }
