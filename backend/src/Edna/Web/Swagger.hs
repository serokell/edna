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
  , ednaApiSwagger
  , ednaAPIWithDocs
  ) where

import Universum

import Data.Swagger (ParamAnySchema(..), ParamLocation(..), ToParamSchema, ToSchema(..), toSchema)
import qualified Data.Swagger as S
import Data.Swagger.Internal.Schema (named)
import qualified Data.Swagger.Internal.Schema as S
import Data.Swagger.Lens as Exports
import Lens.Micro ((?~))
import Lens.Micro.Platform (zoom, (.=), (?=))
import Network.URI (URI)
import Servant (Server, (:<|>)(..))
import Servant.API ((:>))
import Servant.Multipart (MultipartData(..), MultipartForm)
import Servant.Swagger (HasSwagger(..))
import Servant.Swagger.Internal (addParam)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Servant.Swagger.UI.Core (SwaggerUiHtml)

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
  -> S.Swagger
  -> Server api
  -> Server (WithSwaggerUI api)
withSwaggerUI _ swagger server =
  server :<|> swaggerSchemaUIServer swagger

----------------------------------------------------------------------------
-- Edna schema definition
----------------------------------------------------------------------------

-- | Type for Edna API augmented with documentation.
type EdnaAPIWithDocs = WithSwaggerUI EdnaAPI

ednaAPIWithDocs :: Proxy EdnaAPIWithDocs
ednaAPIWithDocs = Proxy

-- | Generates swagger documentation for Edna API.
ednaApiSwagger :: S.Swagger
ednaApiSwagger = executingState (toSwagger ednaAPI) $ do
    zoom S.info $ do
      S.title .= "Edna API"
      S.version .= "1.0.0"
      S.contact ?= mempty `executingState` do
        S.name ?= "Serokell OÜ"
        S.email ?= "hi@serokell.io"
        S.url ?= S.URL "https://serokell.io"

    S.externalDocs ?= mempty `executingState` do
      S.description ?= "Find out more about Swagger"
      S.url .= S.URL "http://swagger.io"

    S.schemes ?= [S.Http, S.Https]

----------------
-- Instances
----------------

instance S.ToSchema URI where
  declareNamedSchema _ = declareNamedSchema @Text Proxy

instance S.ToSchema (SwaggerUiHtml dir api) where
  declareNamedSchema _ =
    S.plain $ mempty `executingState` do
      S.type_ ?= S.SwaggerNull
      S.title ?= "Swagger UI page"

instance S.ToSchema S.Swagger where
  declareNamedSchema _ =
    S.plain $ mempty `executingState` do
      S.type_ ?= S.SwaggerObject
      S.title ?= "Swagger specification"
      S.description ?= "The specification you are currently reading."

instance HasSwagger api =>
         HasSwagger (MultipartForm mem (MultipartData mem) :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api) & addParam param
    where
      param = mempty
        & name .~ "file"
        & required ?~ True
        & description ?~ "Experiment to upload"
        & schema .~ ParamOther (mempty
            & in_ .~ ParamFormData
            & paramSchema .~ (mempty & type_ ?~ S.SwaggerFile))
