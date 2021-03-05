module Edna.ExperimentReader.Error
  ( ExperimentParsingError (..)
  ) where

import Universum

import Codec.Xlsx (ParseError)
import Fmt (Buildable(..), (+|), (|+))

data ExperimentParsingError
  = UnexpectedCellType
  | InvalidCell (Int, Int)
  | EmptyCell
  | WorksheetNotFound
  | TabletStartNotFound
  | FileParsingError ParseError
  | NoConcentrationTablet
  deriving stock (Show, Generic, Eq)

instance Buildable ExperimentParsingError where
  build UnexpectedCellType = "Expected one type of cell but got another one"
  build EmptyCell = "Cell is empty"
  build (InvalidCell e) = "Undefined content of the cell" +| (show e :: String) |+ ""
  build WorksheetNotFound = "Worksheet not found"
  build (FileParsingError e) = show e
  build TabletStartNotFound = "\"<>\" tablet start poin not found"
  build NoConcentrationTablet = "Tablet does not contain concentration"
