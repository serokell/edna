module Edna.ExperimentReader.Error
  ( ExperimentParsingError (..)
  ) where

import Universum

import Codec.Xlsx (CellValue, ParseError)
import Data.Typeable (cast)
import Fmt (Buildable(..), (+|), (|+))

import Edna.ExperimentReader.Types (CellType, PointYX(..))

data ExperimentParsingError
  = forall a . Typeable a => UnexpectedCellType PointYX (CellType a) (Maybe CellValue)
  | InvalidCell PointYX
  | EmptyCell
  | WorksheetNotFound
  | TabletStartNotFound
  | FileParsingError ParseError
  | NoConcentrationTablet

deriving stock instance Show ExperimentParsingError
instance Eq ExperimentParsingError where
  UnexpectedCellType a b c == UnexpectedCellType d e f = a == d && c == f && Just b == (cast e)
  InvalidCell a == InvalidCell b = a == b
  EmptyCell == EmptyCell = True
  WorksheetNotFound == WorksheetNotFound = True
  FileParsingError a == FileParsingError b = a == b
  NoConcentrationTablet == NoConcentrationTablet = True
  _ == _ = False


instance Buildable ExperimentParsingError where
  build (UnexpectedCellType point expected actual) = 
    "Expected \"" +| expected |+ "\" cell type, but found " +| actual |+ " at " +| point |+ ""
  build EmptyCell = "Cell is empty"
  build (InvalidCell e) = "Undefined content of the cell" +| (show e :: String) |+ ""
  build WorksheetNotFound = "Worksheet not found"
  build (FileParsingError e) = show e
  build TabletStartNotFound = "\"<>\" tablet start poin not found"
  build NoConcentrationTablet = "Tablet does not contain concentration results"
