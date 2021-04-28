-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Legacy module that currently defines only 'WithId' type and should probably
-- be changed somehow.

{-# LANGUAGE OverloadedLists #-}

module Edna.Web.Types
  ( WithId (..)

  -- * Re-exported for convenience
  , URI (..)
  ) where

import Universum

import Data.Aeson.TH (deriveToJSON)
import Data.Swagger (SwaggerType(..), ToSchema(..), declareSchemaRef, properties, required, type_)
import Data.Swagger.Internal.Schema (unnamed)
import Fmt (Buildable(..))
import Lens.Micro ((?~))
import Network.URI (URI(..))
import Network.URI.JSON ()
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
