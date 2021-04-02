-- | Bridge types used to communicate between the server app and frontend.

{-# LANGUAGE OverloadedLists #-}

module Edna.Web.Types
  ( WithId (..)
  , StubSortBy (..)

  -- * Re-exported for convenience
  , URI (..)
  ) where

import Universum

import Data.Aeson.TH (deriveToJSON)
import Data.Swagger
  (SwaggerType(..), ToParamSchema(..), ToSchema(..), declareSchemaRef, enum_, properties, required,
  type_)
import Data.Swagger.Internal.Schema (unnamed)
import Fmt (Buildable(..))
import Lens.Micro ((?~))
import Network.URI (URI(..))
import Network.URI.JSON ()
import Servant (FromHttpApiData(..))
import Servant.Util.Combinators.Logging (ForResponseLog(..), buildForResponse, buildListForResponse)

import Edna.Util (SqlId(..), ednaAesonWebOptions)

----------------
-- General types
----------------

-- | This data type is useful when you want to return something with its ID.
data WithId k t = WithId
  { wiId :: SqlId k
  , wItem :: t
  } deriving stock (Generic, Show, Eq)

instance Buildable t => Buildable (WithId k t) where
  build wi = build (wiId wi) <> " " <> build (wItem wi)

instance Buildable t => Buildable (ForResponseLog (WithId k t)) where
  build = buildForResponse

instance Buildable t => Buildable (ForResponseLog [WithId k t]) where
  build = buildListForResponse (take 5)

-- | A stub to specify the sorting order, most likely will be replaced with
-- @servant-util@.
data StubSortBy =
    SortByName
  | SortBySomething

instance Buildable StubSortBy where
  build _ = "STUB"

instance FromHttpApiData StubSortBy where
  parseQueryParam = \case
    "name" -> pure SortByName
    "something" -> pure SortBySomething
    x -> Left $ "unknown sorting order: " <> x

----------------
-- JSON
----------------

deriveToJSON ednaAesonWebOptions ''WithId

----------------
-- Swagger
----------------

instance ToSchema t => ToSchema (WithId k t) where
  declareNamedSchema _ = do
    idSchema <- declareSchemaRef (Proxy :: Proxy Word32)
    itemSchema <- declareSchemaRef (Proxy :: Proxy t)
    pure $ unnamed $ mempty
      & type_ ?~ SwaggerObject
      & properties .~
          [ ("id", idSchema)
          , ("item", itemSchema)
          ]
      & required .~ [ "id", "item" ]

instance ToParamSchema StubSortBy where
  toParamSchema _ = mempty
     & type_ ?~ SwaggerString
     & enum_ ?~ [ "name", "something" ]
