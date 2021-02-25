{-# OPTIONS_GHC -fno-warn-orphans #-}

module Edna.Web.Swagger
  ( WithSwaggerUI
  , withSwaggerUI
  , gDeclareNamedSchema

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
import Data.Swagger.Declare (Declare)
import Data.Swagger.Internal.Schema (named)
import qualified Data.Swagger.Internal.Schema as S
import Data.Swagger.Internal.TypeShape (GenericHasSimpleShape, GenericShape)
import Data.Swagger.Lens as Exports
import qualified GHC.Generics as G
import Lens.Micro ((?~))
import Lens.Micro.Platform (zoom, (.=), (?=))
import Servant ((:<|>)(..), Server)
import Servant.API ((:>))
import Servant.Multipart (MultipartData(..), MultipartForm)
import Servant.Swagger (HasSwagger(..))
import Servant.Swagger.Internal (addParam)
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)
import Servant.Swagger.UI.Core (SwaggerUiHtml)

import Edna.Util (ednaAesonWebOptions)
import Edna.Web.API (EdnaAPI, ednaAPI)
import Edna.Web.Types (ExperimentalMeasurement)


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
-- Instances
----------------------------------------------------------------------------

-- | Schema generation options which match JSON generation options.
schemaOptions :: S.SchemaOptions
schemaOptions = S.fromAesonOptions ednaAesonWebOptions

-- | Default implementation of 'ToSchema' via Generics.
gDeclareNamedSchema
    :: ( Generic a
       , S.GToSchema (G.Rep a)
       , GenericHasSimpleShape a "genericDeclareNamedSchemaUnrestricted" (GenericShape (G.Rep a))
       )
    => Proxy a -> Declare (S.Definitions S.Schema) S.NamedSchema
gDeclareNamedSchema = S.genericDeclareNamedSchema schemaOptions

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

instance S.ToSchema ExperimentalMeasurement where
  declareNamedSchema = gDeclareNamedSchema
