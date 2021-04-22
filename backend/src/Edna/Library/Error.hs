-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Errors that can happen inside Library functionality.

module Edna.Library.Error
  ( LibraryError (..)
  ) where

import Universum

import Fmt (Buildable(..), pretty, (+|), (|+))
import Servant (err400, err404)

import Edna.Util (CompoundId, MethodologyId, ProjectId, TargetId)
import Edna.Web.Error (ToServerError(..), prettyErr)

data LibraryError
  = LETargetNotFound TargetId
  | LECompoundNotFound CompoundId
  | LEMethodologyNotFound MethodologyId
  | LEProjectNotFound ProjectId
  | LEMethodologyNameExists Text
  | LEProjectNameExists Text
  | LEInvalidURI Text
  deriving stock (Show, Eq)

instance Buildable LibraryError where
  build = \case
    LETargetNotFound i -> "Target not found: " <> build i
    LECompoundNotFound i -> "Compound not found: " <> build i
    LEMethodologyNotFound i -> "Methodology not found: " <> build i
    LEProjectNotFound i -> "Project not found: " <> build i
    LEMethodologyNameExists t -> "Methodology with name: `" +| t |+ "` already exists"
    LEProjectNameExists t -> "Project with name: `" +| t |+ "` already exists"
    LEInvalidURI u -> "Invalid URI: " <> build u

instance Exception LibraryError where
  displayException = pretty

instance ToServerError LibraryError where
  toServerError err = case err of
    LETargetNotFound _        -> prettyErr err404 err
    LECompoundNotFound _      -> prettyErr err404 err
    LEMethodologyNotFound _   -> prettyErr err404 err
    LEProjectNotFound _       -> prettyErr err404 err

    LEMethodologyNameExists _ -> prettyErr err400 err
    LEProjectNameExists _     -> prettyErr err400 err
    LEInvalidURI _            -> prettyErr err400 err
