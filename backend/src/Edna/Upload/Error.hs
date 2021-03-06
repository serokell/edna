-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Errors that can happen during file upload.

module Edna.Upload.Error
  ( UploadError (..)
  , UploadApiError (..)
  ) where

import Universum

import Fmt (Buildable(..), pretty)

import Edna.Util (IdType(..), SqlId)
import Edna.Web.Error (ToServerError(..))

-- | Errors that can happen inside Upload service.
data UploadError =
    UEUnknownProject (SqlId 'ProjectId)
  | UEUnknownTestMethodology (SqlId 'MethodologyId)
  deriving stock (Show, Eq)
  deriving anyclass (ToServerError)

instance Buildable UploadError where
  build = \case
    UEUnknownProject i -> "Unknown project: " <> build i
    UEUnknownTestMethodology i -> "Unknown test methodology: " <> build i

instance Exception UploadError where
  displayException = pretty

-- | Errors that can happen inside Upload API.
data UploadApiError
  = NoExperimentFileError
  | TooManyExperimentFilesError
  deriving stock (Show, Generic)
  deriving anyclass (ToServerError)

instance Buildable UploadApiError where
  build = \case
    NoExperimentFileError -> "Experiment file not attached"
    TooManyExperimentFilesError -> "More than one experiment file attached"

instance Exception UploadApiError where
  displayException = pretty
