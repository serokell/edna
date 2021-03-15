{-|
Module which contains the exception classes used in Edna
server handlers and defines the way they are transformed
to Servant errors.
-}

module Edna.Web.Error
  ( ToServerError (..)
  , EdnaServerError (..)
  ) where

import Universum

import Fmt (Buildable(..), pretty, (+|), (|+))
import Servant (ServerError(..), err400)

import Edna.ExperimentReader.Error (ExperimentParsingError(..))

-- Exception thrown by handler
data EdnaServerError
  = XlsxParingError ExperimentParsingError
  | NoExperimentFileError
  | TooManyExperimentFilesError
  deriving stock (Show, Generic)

instance Exception EdnaServerError where
  displayException = pretty

-- | Class of exceptions which can be transformed to @ServerError@
class Exception e => ToServerError e where
    toServerError :: e -> ServerError

instance Buildable EdnaServerError where
  build (XlsxParingError err) = "Xlsx parsing error: "+| err |+""
  build NoExperimentFileError = "Experiment file not attached"
  build TooManyExperimentFilesError = "More than one experiment file attached"

instance ToServerError EdnaServerError where
  toServerError err = err400 { errBody = prettyErr }
    where
      prettyErr = encodeUtf8 @Text $ pretty err
