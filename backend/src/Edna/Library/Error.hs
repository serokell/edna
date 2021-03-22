-- | Errors that can happen inside Target functionality.

module Edna.Library.Error
  ( TargetError (..)
  ) where

import Universum

import Fmt (Buildable(..), pretty)

import Edna.Util (TargetId)
import Edna.Web.Error (ToServerError(..))

data TargetError = TETargetNotFound TargetId
  deriving stock (Show, Eq)
  deriving anyclass (ToServerError)

instance Buildable TargetError where
  build = \case
    TETargetNotFound i -> "Target not found: " <> build i

instance Exception TargetError where
  displayException = pretty
