{-|
Module which contains the exception classes used in Edna
server handlers and defines the way they are transformed
to Servant errors.
-}

module Edna.Web.Error
  ( ToServerError (..)
  , defaultToServerError
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
  toServerError = defaultToServerError

-- | Default implementation of 'toServerError' that returns HTTP code 400
-- (bad request).
defaultToServerError :: Buildable e => e -> ServerError
defaultToServerError err = err400 { errBody = prettyErr }
  where
    prettyErr = encodeUtf8 @Text $ pretty err
