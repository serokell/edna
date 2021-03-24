-- | Errors that can happen inside Library functionality.

module Edna.Library.Error
  ( LibraryError (..)
  ) where

import Universum

import Fmt (Buildable(..), pretty, (+|), (|+))

import Edna.Util (CompoundId, MethodologyId, ProjectId, TargetId)
import Edna.Web.Error (ToServerError(..))

data LibraryError
  = LETargetNotFound TargetId
  | LECompoundNotFound CompoundId
  | LEMethodologyNotFound MethodologyId
  | LEProjectNotFound ProjectId
  | LEMethodologyNameExists Text
  | LEProjectNameExists Text
  | LEInvalidURI Text
  deriving stock (Show, Eq)
  deriving anyclass (ToServerError)

instance Buildable LibraryError where
  build = \case
    LETargetNotFound i -> "Target not found: " <> build i
    LECompoundNotFound i -> "Compound not found: " <> build i
    LEMethodologyNotFound i -> "Methodology not found: " <> build i
    LEProjectNotFound i -> "Project not found: " <> build i
    LEMethodologyNameExists t -> "Methodology with name: " +| t |+ " already exists"
    LEProjectNameExists t -> "Project with name: " +| t |+ " already exists"
    LEInvalidURI u -> "Invlalid URI: " <> build u

instance Exception LibraryError where
  displayException = pretty
