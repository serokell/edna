-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Test.SwaggerSpec
  ( spec
  ) where

import Universum

import Servant.OpenApi (validateEveryToJSON)
import Test.Hspec (Spec, context)

import Edna.Web.API (ednaAPI)
import Edna.Web.Swagger ()

import Test.Gen ()

spec :: Spec
spec = do
  context "ToJSON matches ToSchema" $ validateEveryToJSON ednaAPI
