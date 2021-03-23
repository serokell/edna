-- | Upload-related part of API definition along with implementation.

module Edna.Upload.API
  ( FileUploadEndpoints (..)
  , FileUploadAPI
  , fileUploadEndpoints

  -- * Legacy
  , ExperimentalMeasurement (..)
  , uploadExperiment
  ) where

import Universum

import Data.Aeson.TH (deriveToJSON)
import Data.Swagger (ToSchema(..))
import Servant.API (Capture, JSON, Post, Summary, (:>))
import Servant.API.Generic (AsApi, ToServant, (:-))
import Servant.Multipart (FileData(..), Mem, MultipartData(..), MultipartForm)
import Servant.Server.Generic (AsServerT, genericServerT)

import Edna.ExperimentReader.Parser (parseExperimentXls)
import Edna.ExperimentReader.Types (FileContents(..), Measurement(..), TargetMeasurements(..))
import Edna.Setup (Edna)
import Edna.Upload.Error (UploadApiError(..))
import Edna.Upload.Service (parseFile, uploadFile)
import Edna.Util (ednaAesonWebOptions, gDeclareNamedSchema)
import Edna.Web.Types

-- | Endpoints necessary to implement file uploading.
data FileUploadEndpoints route = FileUploadEndpoints
  { -- | Parse the file and return its summary for preview.
    -- It doesn't change any state, but it's POST because GET can't
    -- receive multipart.
    fueParseFile :: route
      :- "parse"
      :> Summary "Parse the file and return its summary for preview"
      :> MultipartForm Mem (MultipartData Mem)
      :> Post '[JSON] FileSummary

  , -- | Upload the file with some methodology and project.
    fueUploadFile :: route
      :- "upload"
      :> Summary "Upload the file with some methodology and project"
      :> Capture "projectId" (SqlId Project)
      :> Capture "methodologyId" (SqlId TestMethodology)
      :> Capture "description" Text
      :> MultipartForm Mem (MultipartData Mem)
      :> Post '[JSON] FileSummary
  } deriving stock (Generic)

type FileUploadAPI = ToServant FileUploadEndpoints AsApi

fileUploadEndpoints :: ToServant FileUploadEndpoints (AsServerT Edna)
fileUploadEndpoints = genericServerT FileUploadEndpoints
  { fueParseFile = expectOneFile >=> parseFile . snd
  , fueUploadFile = \projectId testMethodologyId description multipart -> do
      (name, contents) <- expectOneFile multipart
      uploadFile projectId testMethodologyId description
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
  fileContents <- either throwM pure (parseExperimentXls file)
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
