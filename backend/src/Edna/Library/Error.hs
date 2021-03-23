-- | Errors that can happen inside Library functionality.

module Edna.Library.Error
  ( LibraryError (..)
  ) where

import Universum

import Fmt (Buildable(..), pretty)

import Edna.Util (CompoundId, TargetId)
import Edna.Web.Error (ToServerError(..))

data LibraryError
  = LETargetNotFound TargetId
  | LECompoundNotFound CompoundId
  | LEInvalidURI Text
  deriving stock (Show, Eq)
  deriving anyclass (ToServerError)

instance Buildable LibraryError where
  build = \case
    LETargetNotFound i -> "Target not found: " <> build i
    LECompoundNotFound i -> "Compound not found: " <> build i
    LEInvalidURI u -> "Invlalid URI: " <> build u

instance Exception LibraryError where
  displayException = pretty
