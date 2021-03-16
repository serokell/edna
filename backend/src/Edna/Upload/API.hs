-- | Upload-related part of API definition along with implementation.

module Edna.Upload.API
  ( FileUploadReq (..)
  , FileUploadEndpoints (..)
  , FileUploadAPI
  , fileUploadEndpoints

  -- * Legacy
  , ExperimentalMeasurement (..)
  , uploadExperiment
  ) where

import Universum

import Data.Aeson.TH (deriveJSON, deriveToJSON)
import Data.Swagger (ToSchema(..))
import Servant.API (Get, JSON, Post, ReqBody, Summary, (:>))
import Servant.API.Generic (AsApi, ToServant, (:-))
import Servant.Multipart (FileData(..), Mem, MultipartData(..), MultipartForm)
import Servant.Server.Generic (AsServerT, genericServerT)

import Edna.ExperimentReader.Parser (parseExperimentXls)
import Edna.ExperimentReader.Types (FileContents(..), Measurement(..), TargetMeasurements(..))
import Edna.Setup (Edna)
import Edna.Upload.Service (parseFile, uploadFile)
import Edna.Util (ednaAesonWebOptions, gDeclareNamedSchema)
import Edna.Web.Error (EdnaServerError(..))
import Edna.Web.Types

-- | Input data submitted along with uploaded file.
data FileUploadReq = FileUploadReq
  { furProject :: SqlId Project
  -- ^ ID of the project the file belongs to.
  , furTestMethodology :: SqlId TestMethodology
  -- ^ ID of the test methodology used throughout the file.
  , furDescription :: Text
  -- ^ Description of the file.
  } deriving stock (Generic, Show)

deriveJSON ednaAesonWebOptions ''FileUploadReq

instance ToSchema FileUploadReq where
  declareNamedSchema = gDeclareNamedSchema

-- | Endpoints necessary to implement file uploading.
data FileUploadEndpoints route = FileUploadEndpoints
  { -- | Parse the file and return its summary for preview.
    fueParseFile :: route
      :- "parse"
      :> Summary "Parse the file and return its summary for preview"
      :> MultipartForm Mem (MultipartData Mem)
      :> Get '[JSON] FileSummary

  , -- | Upload the file with some methodology and project.
    fueUploadFile :: route
      :- "upload"
      :> Summary "Upload the file with some methodology and project"
      :> ReqBody '[JSON] FileUploadReq
      :> MultipartForm Mem (MultipartData Mem)
      :> Post '[JSON] FileSummary
  } deriving stock (Generic)

type FileUploadAPI = ToServant FileUploadEndpoints AsApi

fileUploadEndpoints :: ToServant FileUploadEndpoints (AsServerT Edna)
fileUploadEndpoints = genericServerT FileUploadEndpoints
  { fueParseFile = expectOneFile >=> parseFile . snd
  , fueUploadFile = \FileUploadReq {..} multipart -> do
      (name, contents) <- expectOneFile multipart
      uploadFile furProject furTestMethodology furDescription
        name contents
  }

----------------
-- Helpers
----------------

expectOneFile :: MonadThrow m => MultipartData Mem -> m (Text, LByteString)
expectOneFile multipart = case files multipart of
  [file] -> pure (fdFileName file, fdPayload file)
  [] -> throwM NoExperimentFileError
  _ -> throwM TooManyExperimentFilesError

----------------
-- Legacy
----------------

-- | Legacy type that will be removed soon.
data ExperimentalMeasurement = ExperimentalMeasurement
  { emCompoundId :: Text
  , emTargetId :: Text
  , emConcentration :: Double
  , emSignal :: Double
  , emOutlier :: Bool
  } deriving stock (Generic, Show, Eq)

deriveToJSON ednaAesonWebOptions ''ExperimentalMeasurement

instance ToSchema ExperimentalMeasurement where
  declareNamedSchema = gDeclareNamedSchema

-- Legacy function
uploadExperiment :: MultipartData Mem -> Edna [ExperimentalMeasurement]
uploadExperiment multipart = do
  (fileName, file) <- expectOneFile multipart
  putStrLn $ "Excel file name " ++ show fileName
  fileContents <-
    either (throwM . XlsxParingError) pure (parseExperimentXls file)
  let
    flatten :: (Text, TargetMeasurements) -> [ExperimentalMeasurement]
    flatten (targetName, TargetMeasurements targetMeasurements) =
      flip concatMap (toPairs targetMeasurements) $ \(compoundName, measurements) ->
      flip map measurements $ \Measurement {..} ->
        ExperimentalMeasurement
          { emCompoundId = compoundName
          , emTargetId = targetName
          , emConcentration = mConcentration
          , emSignal = mSignal
          , emOutlier = mIsOutlier
          }
  return $ concatMap flatten $ toPairs $ fcMeasurements fileContents
