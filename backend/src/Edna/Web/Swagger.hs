-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Edna.Web.Swagger
  ( WithSwaggerUI
  , withSwaggerUI

  -- Reexports
  , (?~)
  , named
  , ToParamSchema (..)
  , ToSchema (..)
  , toSchema
  , module Exports

  -- Edna specific
  , EdnaAPIWithDocs
  , ednaApiSwagger
  , ednaAPIWithDocs
  ) where

import Universum

import qualified Data.OpenApi as O
import qualified Data.OpenApi.Internal.Schema as O
import qualified Data.Text as T

import Control.Lens (zoom, (.=), (?=), (?~))
import Data.OpenApi (ToParamSchema, ToSchema(..), toSchema)
import Data.OpenApi.Internal.Schema (named)
import Data.OpenApi.Lens as Exports
import Data.Version (showVersion)
import Network.URI (URI)
import Servant (Server, (:<|>)(..))
import Servant.OpenApi (HasOpenApi(..))
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Servant.Swagger.UI.Core (SwaggerUiHtml)

import qualified Paths_edna as Meta

import Edna.Web.API (EdnaAPI, ednaAPI)

----------------------------------------------------------------------------
-- Generic definitions
----------------------------------------------------------------------------

-- | Swagger UI we use across the project.
type SwaggerUI =
  -- Tag "Documentation" :>
  SwaggerSchemaUI "docs" "swagger.json"

-- | Attach a swagger UI to the given API.
type WithSwaggerUI api = api :<|> SwaggerUI

-- | Attach an UI serving given documentation to the given server.
withSwaggerUI
  :: Proxy api
  -> O.OpenApi
  -> Server api
  -> Server (WithSwaggerUI api)
withSwaggerUI _ swagger eServer = eServer :<|> swaggerSchemaUIServer swagger

----------------------------------------------------------------------------
-- Edna schema definition
----------------------------------------------------------------------------

-- | Type for Edna API augmented with documentation.
type EdnaAPIWithDocs = WithSwaggerUI EdnaAPI

ednaAPIWithDocs :: Proxy EdnaAPIWithDocs
ednaAPIWithDocs = Proxy

-- | Generates swagger documentation for Edna API.
ednaApiSwagger :: O.OpenApi
ednaApiSwagger = executingState (toOpenApi ednaAPI) $ do
    zoom O.info $ do
      O.title .= "Edna API"
      O.version .= T.pack (showVersion Meta.version)
      O.license ?= "AGPL-3.0-or-later"
      O.contact ?= mempty `executingState` do
        O.name ?= "Serokell OÜ"
        O.email ?= "hi@serokell.io"
        O.url ?= O.URL "https://serokell.io"

    O.externalDocs ?= mempty `executingState` do
      O.description ?= "Find out more about Swagger"
      O.url .= O.URL "http://swagger.io"

----------------
-- Instances
----------------

instance ToSchema URI where
  declareNamedSchema _ = declareNamedSchema @Text Proxy

instance ToSchema (SwaggerUiHtml dir api) where
  declareNamedSchema _ =
    O.plain $ mempty `executingState` do
      O.type_ ?= O.OpenApiNull
      O.title ?= "Swagger UI page"

instance ToSchema O.OpenApi where
  declareNamedSchema _ =
    O.plain $ mempty `executingState` do
      O.type_ ?= O.OpenApiObject
      O.title ?= "Swagger specification"
      O.description ?= "The specification you are currently reading."
