-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for types from other packages.

module Edna.Orphans () where

import Universum

import qualified Data.OpenApi as O
import qualified Data.Text as T

import Data.Aeson (Value)
import Data.Aeson.Text (encodeToTextBuilder)

import Control.Lens ((<>~), (?~))
import Fmt (Buildable(..))
import Lens.Micro.Internal (Field1(..))
import RIO (RIO(..))
import Servant.API ((:>))
import Servant.Multipart (MultipartForm')
import Servant.OpenApi (HasOpenApi(..))
import Servant.Util.Combinators.Logging (ApiCanLogArg, ForResponseLog(..))
import Servant.Util.Combinators.Pagination (PaginationParams)
import Servant.Util.Combinators.Sorting.Base (SortingParams)
import Servant.Util.Common (ReifyParamsNames(..), reifyParamsNames)
import Servant.Util.Common.Common (ApiHasArgClass(..))

-- It's also available in @rio-orphans@, but that would add extra dependencies,
-- so it's simpler to just have one line for now.
deriving newtype instance MonadCatch (RIO env)

----------------
-- Logging
----------------

instance ApiHasArgClass (MultipartForm' mods tag t) where
  type ApiArg (MultipartForm' mods tag t) = t
  apiArgName _ = "multipart"

instance ApiCanLogArg (MultipartForm' mods tag t)

-- defined the same way as in @Lens.Micro.Internal@ (where instances are
-- provided only for tuples with up to 5 items)
instance Field1 (a, b, c, d, e, f, g) (a', b, c, d, e, f, g) a a' where
  _1 k ~(a, b, c, d, e, f, g) = (, b, c, d, e, f, g) <$> k a
  {-# INLINE _1 #-}

----------------
-- OpenAPI 3
----------------
instance Buildable (ForResponseLog O.OpenApi) where
  build _ = "Swagger specification"

instance Buildable (ForResponseLog Value) where
  build (ForResponseLog v) = encodeToTextBuilder v

instance (HasOpenApi api, ReifyParamsNames params)
      => HasOpenApi (SortingParams params :> api) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy api)
    & O.allOperations . O.parameters <>~ [O.Inline param]
    where
      param :: O.Param
      param = mempty
          & O.name .~ "sortBy"
          & O.description ?~ T.unlines
            [ "Allows lexicographical sorting on fields."
            , "General format is one of:"
            , "  * `+field1,-field2`"
            , "  * `asc(field1),desc(field2)`"
            , ""
            , " Fields allowed for this endpoint: " <> allowedFieldsDesc
            ]
          & O.required ?~ False
          & O.in_ .~ O.ParamQuery
          & O.schema ?~ paramSchema

      paramSchema :: O.Referenced O.Schema
      paramSchema = O.Inline $ mempty
        & O.type_ ?~ O.OpenApiString
        & O.pattern ?~ "^"
                    <> fieldPattern
                    <> "(,"
                    <> fieldPattern
                    <> ")*"
                    <> "$"

      fieldPattern :: Text
      fieldPattern = "("
                  <> "[+-]("
                  <> allowedFieldsPattern
                  <> ")+"
                  <> "|"
                  <> "(asc|desc)\\(("
                  <> allowedFieldsPattern
                  <> ")+\\))"

      allowedFields :: [Text]
      allowedFields = reifyParamsNames @params

      allowedFieldsDesc :: Text
      allowedFieldsDesc = T.intercalate ", " $
        map ((<> "`") . ("`" <>)) (toList allowedFields)

      allowedFieldsPattern :: Text
      allowedFieldsPattern = T.intercalate "|" (toList allowedFields)


instance HasOpenApi api => HasOpenApi (PaginationParams :> api) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy api)
    & O.allOperations . O.parameters <>~ [ O.Inline offsetParam
                                         , O.Inline limitParam
                                         ]
    where
      offsetParam :: O.Param
      offsetParam = mempty
        & O.name .~ "offset"
        & O.description ?~ "Pagination parameter. How many items to skip from \
                           \the beginning."
        & O.required ?~ False
        & O.in_ .~ O.ParamQuery
        & O.schema ?~ offsetParamSchema

      offsetParamSchema :: O.Referenced O.Schema
      offsetParamSchema = O.Inline $ mempty
        & O.type_ ?~ O.OpenApiInteger
        & O.format ?~ "int32"

      limitParam :: O.Param
      limitParam = mempty
        & O.name .~ "limit"
        & O.description ?~ "Pagination parameter. Maximum number of items to return."
        & O.required ?~ False
        & O.in_ .~ O.ParamQuery
        & O.schema  ?~ limitParamSchema

      limitParamSchema :: O.Referenced O.Schema
      limitParamSchema = O.Inline $ mempty
        & O.type_ ?~ O.OpenApiInteger
        & O.format ?~ "int32"
