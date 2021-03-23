module Edna.ExperimentReader.Parser
  ( parseExperimentXls
  , processWorkSheet
  , ParserType
  ) where

import Universum

import qualified Data.ByteString.Lazy as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M

import Codec.Xlsx (CellValue(..), Worksheet(..), cellValue, ixCell, toXlsxEither, wsCells, xlSheets)
import Data.List.Split (wordsBy)
import Data.Text (replace)
import Fmt (pretty)
import Lens.Micro (ix, _Just)
import Text.Read (readParen)

import Edna.ExperimentReader.Error (ExperimentParsingError(..))
import Edna.ExperimentReader.Types
  (CellType(..), Parameter(..), ParameterType(..), PointYX(..), Signal(..), PlateUnit(..))
import Edna.Web.Types (ExperimentalMeasurement(..))

type ParserType a = Either ExperimentParsingError a

-- | Based on specific CellType return its value or get and exception that cell is empty or its
-- expected type does not satisfy actual type
specificCellAt
  :: Typeable a
  => Worksheet
  -> PointYX
  -> CellType a
  -> Either ExperimentParsingError a
specificCellAt workSheet p@(PointYX position) cellType =
  let cell = workSheet ^? ixCell position . cellValue . _Just in
  case (cellType, cell) of
    (_, Nothing) -> Left EmptyCell
    (_, Just (CellText "")) -> Left EmptyCell

    (CText, Just (CellText x)) -> Right x
    (CDouble, Just (CellDouble x)) -> Right x

    (CSignal, Just (CellDouble x)) -> Right $ Signal x False
    (CSignal, Just (CellText
      -- If signal value is in brackets then it is outlier
      (readParen True reads . toString . replace "," "." -> ((d, "") : _))
      )) -> Right $ Signal d True

    (_, cv) -> Left $ UnexpectedCellType p cellType cv

-- | Check that cell representation equals to some value
cellSatisfy :: Worksheet -> Text -> PointYX -> Bool
cellSatisfy workSheet value (PointYX position) = case workSheet ^? ixCell position . cellValue of
  Just (Just x) -> pretty x == value
  _ -> value == ""

-- | Common function for splitting
paramsSplit :: Worksheet -> ParameterType -> Int -> NonEmpty Int -> ParserType [Parameter]
paramsSplit workSheet pType normalAxisPoint indexes = constructParams (last indexes) $
    flip NE.filter indexes $ \c -> not $ cellSatisfy workSheet "" $ direction (normalAxisPoint, c)
  where
    direction :: (Int, Int) -> PointYX
    direction points = case pType of
      Target -> PointYX points
      Compound -> PointYX $ swap points

    -- | Construct params from their indexes
    constructParams :: Int -> [Int] -> ParserType [Parameter]
    constructParams _ [] = pure []
    constructParams lastId [idx] = do
      name <- specificCellAt workSheet (direction (normalAxisPoint, idx)) CText
      pure [Parameter name (idx, lastId)]
    constructParams lastId (id1 : id2 : ids) = do
      name <- specificCellAt workSheet (direction (normalAxisPoint, id1)) CText
      restParams <- constructParams lastId (id2 : ids)
      pure $ Parameter name (id1, id2 - 1) : restParams

parseExperimentXls :: L.ByteString -> ParserType [ExperimentalMeasurement]
parseExperimentXls content = do
  xlsx <- first FileParsingError $ toXlsxEither content
  workSheet <- maybeToRight WorksheetNotFound $ xlsx ^? xlSheets . ix 0 . _2
  processWorkSheet workSheet

processWorkSheet :: Worksheet -> ParserType [ExperimentalMeasurement]
processWorkSheet workSheet = do
  -- Check that plate exists and start from the top left corner of the table
  unless (cellSatisfy workSheet "<>" $ PointYX (1, 1)) $ Left PlateStartNotFound

  -- Compute height and width of the sheet (width also equals to plateWidth)
  let (shtHeight, plateWidth) =
        foldl' (\(y, x) (y', x') -> (max y y', max x x')) (1, 1) $ M.keys $ workSheet ^. wsCells

  -- Compute plate height finding the second plate starting point
  plateHeight <- case [c | c <- [2..shtHeight], cellSatisfy workSheet "<>" $ PointYX (c, 1)] of
    [c] -> Right $ c - 1
    _ -> Left NoConcentrationPlate

  -- Get list of indexes list of plate units where unit is a part of the plate separated by
  -- empty cell (in the column of these cells are compound names)
  -- Also convert list of lists to the list of NonEmpty
  let plateUnitsIndexes = foldr (\u us -> case u of {[] -> us; (x : xs) -> (x :| xs) : us}) [] $
        wordsBy (\c -> cellSatisfy workSheet "" (PointYX (1, c))) [2..plateWidth]

  -- For each unit find targets and compounds (their names and indexes range)
  plateUnits <- forM plateUnitsIndexes $ \unit -> do
    tuTargets <- paramsSplit workSheet Target 2 unit
    -- Compound size always equals to 3, so it is needed to set it after split
    tuCompounds <- map (\p@(pIndexes -> (s, _)) -> p {pIndexes = (s, s + 2)}) <$>
      paramsSplit workSheet Compound (head unit - 1) (3 :| [4..plateHeight])
    pure PlateUnit{..}

  -- For each unit, for each target and compound find corresponding values
  sequenceA $ filter (either (/= EmptyCell) (const True)) $ do
    PlateUnit {..} <- plateUnits
    target <- tuTargets
    compound <- tuCompounds
    x <- [fst $ pIndexes target .. snd $ pIndexes target]
    y <- [fst $ pIndexes compound .. snd $ pIndexes compound]
    pure $ do
      Signal{..} <- specificCellAt workSheet (PointYX (y, x)) CSignal
      concentration <- specificCellAt workSheet (PointYX (y + plateHeight - 1, x)) CDouble
      pure $ ExperimentalMeasurement (pName compound) (pName target) concentration sValue sOutlier
