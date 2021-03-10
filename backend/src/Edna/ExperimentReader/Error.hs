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
  | EmptyCell PointYX
  | WorksheetNotFound
  | PlateStartNotFound
  | FileParsingError ParseError
  | NoConcentrationPlate
  deriving anyclass Exception

deriving stock instance Show ExperimentParsingError
instance Eq ExperimentParsingError where
  UnexpectedCellType a b c == UnexpectedCellType d e f = a == d && c == f && Just b == cast e
  EmptyCell a == EmptyCell b = a == b
  WorksheetNotFound == WorksheetNotFound = True
  FileParsingError a == FileParsingError b = a == b
  NoConcentrationPlate == NoConcentrationPlate = True
  PlateStartNotFound == PlateStartNotFound = True
  UnexpectedCellType {} == _ = False
  EmptyCell _ == _ = False
  WorksheetNotFound == _ = False
  FileParsingError _ == _ = False
  NoConcentrationPlate == _ = False
  PlateStartNotFound == _ = False

instance Buildable ExperimentParsingError where
  build (UnexpectedCellType point expected actual) =
    "Expected \"" +| expected |+ "\" cell type, but found " +| actual |+ " at " +| point |+ ""
  build (EmptyCell point) = "Cell at " +| point |+ " is empty"
  build WorksheetNotFound = "Worksheet not found"
  build (FileParsingError e) = show e
  build PlateStartNotFound = "\"<>\" plate start point not found"
  build NoConcentrationPlate = "Plate does not contain concentration results"
