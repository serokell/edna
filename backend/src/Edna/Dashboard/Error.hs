-- | Errors that can happen inside Dashboard functionality.

module Edna.Dashboard.Error
  ( DashboardError (..)
  ) where

import Universum

import Fmt (Buildable(..), pretty)
import Servant (ServerError(..), err400, err404)

import Edna.Util (ExperimentId, SubExperimentId)
import Edna.Web.Error (ToServerError(..))

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
    DESubExperimentNotFound _ -> err404 { errBody = prettyErr }
    DEExperimentNotFound _ -> err404 { errBody = prettyErr }
    DECantDeletePrimary _ -> err400 { errBody = prettyErr }
    where
      prettyErr = encodeUtf8 @Text $ pretty err
