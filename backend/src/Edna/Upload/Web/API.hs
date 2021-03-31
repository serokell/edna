-- | Upload-related part of API definition along with implementation.

module Edna.Upload.Web.API
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
import Fmt (Buildable(..))
import Servant.API (JSON, Post, Summary, (:>))
import Servant.API.Generic (AsApi, ToServant, (:-))
import Servant.Multipart (Mem, MultipartForm)
import Servant.Server.Generic (AsServerT, genericServerT)
import Servant.Util.Combinators.Logging (ForResponseLog(..))

import Edna.ExperimentReader.Parser (parseExperimentXls)
import Edna.ExperimentReader.Types (FileContents(..), Measurement(..), TargetMeasurements(..))
import Edna.Setup (Edna)
import Edna.Upload.Service (parseFile, uploadFile)
import Edna.Upload.Web.Types (FileBS(..), FileSummary, FileUploadReq(..))
import Edna.Util (ednaAesonWebOptions, gDeclareNamedSchema)

-- | Endpoints necessary to implement file uploading.
data FileUploadEndpoints route = FileUploadEndpoints
  { -- | Parse the file and return its summary for preview.
    -- It doesn't change any state, but it's POST because GET can't
    -- receive multipart.
    fueParseFile :: route
      :- "parse"
      :> Summary "Parse the file and return its summary for preview"
      :> MultipartForm Mem FileBS
      :> Post '[JSON] FileSummary

  , -- | Upload the file with some methodology and project.
    fueUploadFile :: route
      :- "upload"
      :> Summary "Upload the file with some methodology and project"
      :> MultipartForm Mem FileUploadReq
      :> Post '[JSON] FileSummary
  } deriving stock (Generic)

type FileUploadAPI = ToServant FileUploadEndpoints AsApi

fileUploadEndpoints :: ToServant FileUploadEndpoints (AsServerT Edna)
fileUploadEndpoints = genericServerT FileUploadEndpoints
  { fueParseFile = parseFile . fbsFile
  , fueUploadFile = \FileUploadReq{furFile = FileBS{..}, ..} ->
      uploadFile furProject furTestMethodology (fromMaybe "" furDescription) fbsName fbsFile
  }

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

instance Buildable (ForResponseLog [ExperimentalMeasurement]) where
  build _ = "LEGACY ExperimentalMeasurement"

deriveToJSON ednaAesonWebOptions ''ExperimentalMeasurement

instance ToSchema ExperimentalMeasurement where
  declareNamedSchema = gDeclareNamedSchema

-- Legacy function
uploadExperiment :: FileBS -> Edna [ExperimentalMeasurement]
uploadExperiment FileBS{..} = do
  putStrLn $ "Excel file name " ++ show fbsName
  fileContents <- either throwM pure (parseExperimentXls fbsFile)
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
