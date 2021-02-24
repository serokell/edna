{-|
Module which contains the exception classes used in Edna
server handlers and defines the way they are transformed
to Servant errors.
-}

module Edna.Web.Error
  ( ToServerError (..)
  , EdnaServerError (..)
  , ExperimentParsingError (..)
  ) where

import Universum

import Fmt (Buildable(..), pretty, (+|), (|+))
import Servant (ServerError(..), err400)

-- Exception thrown by handler
data EdnaServerError
  = XlsxParingError ExperimentParsingError
  | NoExperimentFileError
  deriving stock (Show, Generic)

instance Exception EdnaServerError

data ExperimentParsingError
  = UnexpectedCellType
  | InvalidCell
  | WorksheetNotFound Text
  deriving stock (Show, Generic)

-- | Class of exceptions which can be transformed to @ServerError@
class Exception e => ToServerError e where
    toServerError :: e -> ServerError

instance Buildable EdnaServerError where
  build (XlsxParingError err) = "Xlsx parsing error: "+| err |+""
  build NoExperimentFileError = "Experiment file not attached"

instance Buildable ExperimentParsingError where
  build UnexpectedCellType = "Expected one type of cell but got another one"
  build InvalidCell = "Undefined content of the cell"
  build (WorksheetNotFound w) = w |+ " worksheet not found"

instance ToServerError EdnaServerError where
  toServerError er@(XlsxParingError _) =
    err400 { errBody = encodeUtf8 (pretty @EdnaServerError @Text er) }
  toServerError er@NoExperimentFileError =
    err400 { errBody = encodeUtf8 (pretty @EdnaServerError @Text er) }
