-- | Errors that can happen inside Dashboard functionality.

module Edna.Dashboard.Error
  ( DashboardError (..)
  ) where

import Universum

import Fmt (Buildable(..), pretty)
import Servant (ServerError(..), err404)

import Edna.Util (SubExperimentId)
import Edna.Web.Error (ToServerError(..))

data DashboardError
  = DESubExperimentNotFound SubExperimentId
  deriving stock (Show, Eq)

instance Buildable DashboardError where
  build = \case
    DESubExperimentNotFound i -> "Sub-experiment not found: " <> build i

instance Exception DashboardError where
  displayException = pretty

instance ToServerError DashboardError where
  toServerError err = case err of
    DESubExperimentNotFound _ -> err404 { errBody = prettyErr }
    where
      prettyErr = encodeUtf8 @Text $ pretty err
