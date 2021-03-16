module Test.ParserSpec
  ( spec
  ) where

import Universum

import qualified Data.ByteString.Lazy as L
import qualified Data.Set as S

import Codec.Xlsx (CellValue(..), Worksheet, ixSheet)
import Codec.Xlsx.Parser (toXlsx)
import System.FilePath ((</>))
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldThrow)

import Edna.ExperimentReader.Error (ExperimentParsingError(..))
import Edna.ExperimentReader.Parser (processWorkSheet)
import Edna.ExperimentReader.Types
  (CellType(..), FileContents(..), Measurement(..), PointYX(..), TargetMeasurements(..))

tablesPath :: FilePath
tablesPath = "resources" </> "testSheets"

getWorkSheet :: FilePath -> Text -> IO Worksheet
getWorkSheet filePath sheetName = do
  file <- L.readFile filePath
  maybe (throwM WorksheetNotFound) pure $ toXlsx file ^? ixSheet sheetName

tableBaseTest :: FilePath -> Text -> [Text] -> [Text] -> Int -> Int -> Expectation
tableBaseTest path sheetName eCompounds eTargets eOutliers eNumberOfMeasurements = do
  workSheet <- getWorkSheet (tablesPath </> path) sheetName
  let parsingResultEither = processWorkSheet workSheet
  parsingResult <- either throwM pure parsingResultEither
  let
    pairMeasurements :: [(Text, TargetMeasurements)]
    pairMeasurements= toPairs $ fcMeasurements parsingResult
  let targets = sort $ map fst pairMeasurements
  let
    flatTargetMeasurements :: [(Text, [Measurement])]
    flatTargetMeasurements =
      concatMap (toPairs . unTargetMeasurements . snd) pairMeasurements
  let compounds = S.elems . S.fromList . map fst $ flatTargetMeasurements
  let
    flatMeasurements :: [Measurement]
    flatMeasurements = concatMap snd flatTargetMeasurements
  let outliers = length $ filter mIsOutlier flatMeasurements
  let numberOfMeasurements = length flatMeasurements
  targets `shouldBe` eTargets
  compounds `shouldBe` eCompounds
  outliers `shouldBe` eOutliers
  numberOfMeasurements `shouldBe` eNumberOfMeasurements

spec :: Spec
spec = do
  describe "Example tables" $ do
    it "First table" $
      tableBaseTest ("exampleSheets" </> "Ex1.xlsx") "Magellan Sheet 1"
        ["MOL100","MOL11","MOL123","MOL322","MOL456","MOL900"]
        ["PATAK-1"]
        0
        170
    it "Second table" $
      tableBaseTest ("exampleSheets" </> "Ex2.xlsx") "Magellan Sheet 1"
        ["MOL00","MOL12","MOLaa (aga)","MOLab","MOLbb (aga)","MOLyar"]
        ["KKKarta"]
        0
        180
    it "Third table" $
      tableBaseTest ("exampleSheets" </> "Ex3.xlsx") "Magellan Sheet 1"
        ["MOL","MOL-MOL","MOL10-foo-bar","MOL9-foo-bar","MOLPOG-122","MOLPOGKAR"]
        ["QWE-12"]
        15
        180
    it "Fourth table" $
      tableBaseTest ("exampleSheets" </> "Ex4.xlsx") "Magellan Sheet 1"
        ["MOL098","MOLrad","MOLyad"] ["Pol16","Pol8"]
        0
        216
  describe "Valid tables" $ do
    it "Small plate" $
      tableBaseTest "Valid.xlsx" "SmallPlate" ["A"] ["A"] 0 3
    it "Empty plate" $
      tableBaseTest "Valid.xlsx" "EmptyPlate" [] [] 0 0
    it "Complex plate" $
      tableBaseTest "Valid.xlsx" "Complex" ["A","B"] ["A","B","C","D"] 9 32
  describe "Invalid tables" $ do
    let expectErr res errExpected = case res of
          Left err
            | err == errExpected -> pass
            | otherwise -> expectationFailure $ "Unexpected error: " <> show err
          Right x -> expectationFailure $ "Expected error but got: " <> show x
    it "No plate start" do
      workSheet <- getWorkSheet (tablesPath </> "Invalid.xlsx") "NoPlateStart"
      let parsingResult = processWorkSheet workSheet
      expectErr parsingResult PlateStartNotFound
    it "No concentration plate" do
      workSheet <- getWorkSheet (tablesPath </> "Invalid.xlsx") "NoConcentrationPlate"
      let parsingResult = processWorkSheet workSheet
      expectErr parsingResult NoConcentrationPlate
    it "No work sheet" do
      getWorkSheet (tablesPath </> "Invalid.xlsx") "NoSuchWorkSheet" `shouldThrow`
        (== WorksheetNotFound)
    it "Unexpected cell type" do
      workSheet <- getWorkSheet (tablesPath </> "Invalid.xlsx") "UnexpectedType"
      let parsingResult = processWorkSheet workSheet
      expectErr parsingResult
        (UnexpectedCellType (PointYX (3,3)) CSignal $ Just $ CellText "A")
