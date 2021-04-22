-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-|
Module which contains the exception classes used in Edna
server handlers and defines the way they are transformed
to Servant errors.
-}

module Edna.Web.Error
  ( ToServerError (..)
  , prettyErr
  ) where

import Universum

import Fmt (Buildable, pretty)
import Servant (ServerError(..), err400)

-- | Class of exceptions which can be transformed to 'ServerError'.
-- Default implementation transforms to 400 HTTP code which is a good
-- default but sometimes you may want something else.
class Exception e => ToServerError e where
  toServerError :: e -> ServerError

  default toServerError :: Buildable e => e -> ServerError
  toServerError = prettyErr err400

-- | Helper for creating toServerError' implementations
prettyErr :: Buildable e => ServerError -> e -> ServerError
prettyErr statusCode err = statusCode
  { errBody = encodeUtf8 @Text $ pretty err
  }
