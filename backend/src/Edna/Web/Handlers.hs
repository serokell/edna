module Edna.Web.Handlers
  ( ednaHandlers
  , EdnaServerError (..)
  , ExperimentParsingError (..)
  )
where

import Universum
import qualified Universum.Unsafe as U

import Codec.Xlsx (Cell(..), CellValue(..), Worksheet(..), Xlsx, atSheet, toXlsx)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Servant.API.Generic (ToServant)
import Servant.Multipart (FileData(..), Mem, MultipartData(..))
import Servant.Server (Handler)
import Servant.Server.Generic (AsServerT, genericServerT)

import Edna.Web.Error (EdnaServerError (..), ExperimentParsingError (..))
import Edna.Web.API (EdnaEndpoints(..))
import Edna.Web.Types (ExperimentalMeasurement(..))

type EdnaHandlers m = ToServant EdnaEndpoints (AsServerT m)

-- | Server handler implementation for Edna API.
ednaHandlers :: EdnaHandlers Handler
ednaHandlers =
  genericServerT
    EdnaEndpoints
      { eeUploadExperiment = uploadExperiment
      }

uploadExperiment :: MultipartData Mem -> Handler [ExperimentalMeasurement]
uploadExperiment multipart = do
  let file = U.head (files multipart)
  putStrLn $ "Excel file name " ++ show (fdFileName file)
  either (throwM . XlsxParingError) pure (parseExperimentXls $ fdPayload file)

type ParserType a = Either ExperimentParsingError a

parseExperimentXls :: L.ByteString -> ParserType [ExperimentalMeasurement]
parseExperimentXls content = do
  -- "Signal", "Concentration", "Compound"
  let readSheet' :: Text -> (CellValue -> ParserType a) -> ParserType (Map (Int, Int) a)
      readSheet' = readSheet (toXlsx content)

  compounds <-
    readSheet'
      "Compound"
      ( \case
          CellText x -> Right x
          _ -> Left UnexpectedCellType
      )

  concentrations <-
    readSheet'
      "Concentration"
      ( \case
          CellDouble x -> Right x
          _ -> Left UnexpectedCellType
      )

  signals <-
    readSheet'
      "Signal"
      ( \case
          CellDouble x -> Right x
          _ -> Left UnexpectedCellType
      )

  pure $ mergeCells compounds concentrations signals
  where
    readSheet
      :: Xlsx
      -> Text
      -> (CellValue -> ParserType a)
      -> ParserType (Map (Int, Int) a)
    readSheet xlsx sheetName unwrap = do
      sheet <- maybe (Left $ WorksheetNotFound sheetName) Right (xlsx ^. atSheet sheetName)
      traverse (maybe (Left InvalidCell) unwrap . _cellValue) (_wsCells sheet)

    mergeCells
      :: Map (Int, Int) Text
      -> Map (Int, Int) Double
      -> Map (Int, Int) Double
      -> [ExperimentalMeasurement]
    mergeCells compounds concentrations signals =
      mapMaybe
        (\(rc, cmp) -> ExperimentalMeasurement cmp <$> M.lookup rc concentrations <*> M.lookup rc signals)
        (M.toList compounds)
