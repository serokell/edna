-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for types from other packages.

module Edna.Orphans () where

import Universum

import qualified Data.CaseInsensitive as CI
import Data.Swagger (SwaggerType(..), ToParamSchema(..), format, type_)
import Fmt (Buildable(..), Builder, blockListF, fmt, listF, tupleF)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Lens.Micro ((?~))
import RIO (RIO(..))
import Servant.API (Header)
import Servant.API.ResponseHeaders (GetHeaders', Headers, getHeaders, getResponse)
import Servant.Multipart (MultipartForm')
import Servant.Pagination (AcceptRanges, ContentRange, Ranges)
import Servant.Util.Combinators.Logging (ApiCanLogArg, ForResponseLog, buildForResponse)
import Servant.Util.Common.Common (ApiHasArgClass(..), symbolValT)

-- It's also available in @rio-orphans@, but that would add extra dependencies,
-- so it's simpler to just have one line for now.
deriving newtype instance MonadCatch (RIO env)

----------------
-- Logging (servant-util)
----------------

instance ApiHasArgClass (MultipartForm' mods tag t) where
  type ApiArg (MultipartForm' mods tag t) = t
  apiArgName _ = "multipart"

instance ApiCanLogArg (MultipartForm' mods tag t)

-- TODO [EDNA-87] Instances like this one should be in @servant-util@.
instance KnownSymbol sym => ApiHasArgClass (Header sym a) where
  type ApiArg (Header sym a) = Maybe a
  apiArgName _ = symbolVal @sym Proxy <> " header"

instance KnownSymbol sym => ApiCanLogArg (Header sym a)

instance Show (Ranges fields resource) => Buildable (Ranges fields resource) where
  -- It's a bit hard to define something better, so we defer to @show@ for now.
  build = build . show @String

instance (GetHeaders' headers, Buildable a) => Buildable (Headers headers a) where
  build headers =
    build (getResponse headers) <>
    " with headers\n" <> blockListF (map buildHeader untypedHeaders)
    where
      untypedHeaders = getHeaders headers

      -- Headers are usually human-readable so applying @show@ should be ok.
      buildHeader :: (CI.CI ByteString, ByteString) -> Builder
      buildHeader (CI.original -> headerName, headerBody) =
        tupleF (show @String headerName, show @String headerBody)

instance (GetHeaders' headers, Buildable a) => Buildable (ForResponseLog $ Headers headers a) where
  build = buildForResponse

----------------
-- Swagger
----------------

-- TODO [EDNA-97] These instances will be improved once we switch to @openapi3@.
instance ToParamSchema (ContentRange fields resource) where
  toParamSchema Proxy = mempty
    & type_ ?~ SwaggerString

instance ToParamSchema (AcceptRanges fields) where
  toParamSchema Proxy = mempty
    & type_ ?~ SwaggerString
    & format ?~ "A comma-separated list of fields"

class KnownSymbols (fields :: [Symbol]) where
  symbolVals :: [Text]

instance KnownSymbols '[] where
  symbolVals = []

instance (KnownSymbol field, KnownSymbols fields) => KnownSymbols (field ': fields) where
  symbolVals = symbolValT @field : symbolVals @fields

instance KnownSymbols fields => ToParamSchema (Ranges fields resource) where
  toParamSchema Proxy = mempty
    & format ?~
      "<field> [<value>][; offset <o>][; limit <l>][; order <asc|desc>] where <field> is one of "
      <> fmt (listF (symbolVals @fields))
