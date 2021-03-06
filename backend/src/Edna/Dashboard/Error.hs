-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Errors that can happen inside Dashboard functionality.

module Edna.Dashboard.Error
  ( DashboardError (..)
  ) where

import Universum

import Fmt (Buildable(..), pretty)
import Servant (err400, err404)

import Edna.Util (ExperimentId, SubExperimentId)
import Edna.Web.Error (ToServerError(..), prettyErr)

data DashboardError
  = DESubExperimentNotFound SubExperimentId
  | DEExperimentNotFound ExperimentId
  | DECantDeletePrimary SubExperimentId
  deriving stock (Show, Eq)

instance Buildable DashboardError where
  build = \case
    DESubExperimentNotFound i -> "Sub-experiment not found: " <> build i
    DEExperimentNotFound i -> "Experiment not found: " <> build i
    DECantDeletePrimary i ->
      "It's not allowed to delete primary sub-experiments (" <> build i <>
      "), please choose the new primary one first"

instance Exception DashboardError where
  displayException = pretty

instance ToServerError DashboardError where
  toServerError err = case err of
    DESubExperimentNotFound _ -> prettyErr err404 err
    DEExperimentNotFound _    -> prettyErr err404 err
    DECantDeletePrimary _     -> prettyErr err400 err
