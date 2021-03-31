module Edna.Upload.Web.Types
  ( FileSummary (..)
  , NameAndId (..)
  , FileSummaryItem (..)
  , FileBS (..)
  , FileUploadReq (..)
  ) where

import Universum

import qualified Data.Swagger as S

import Data.Aeson (ToJSON)
import Data.Aeson.TH (deriveToJSON)
import Data.Swagger
  (NamedSchema(..), ParamAnySchema(..), ParamLocation(..), ToSchema(..), binarySchema, description,
  in_, name, paramSchema, required, schema, type_)
import Fmt (Buildable(..), pretty, (+|), (|+))
import Lens.Micro ((?~))
import Servant.API ((:>))
import Servant.Multipart
  (FromMultipart(..), Mem, MultipartData, MultipartForm, fdFileName, fdPayload, files, lookupInput)
import Servant.Swagger.Internal (HasSwagger(..), addParam)
import Servant.Util.Combinators.Logging (ForResponseLog(..), buildForResponse, buildListForResponse)

import Edna.Upload.Error (UploadApiError(..))
import Edna.Util
  (IdType(..), MethodologyId, ProjectId, SqlId(..), ednaAesonWebOptions, gDeclareNamedSchema)

expectOneFile :: MultipartData Mem -> Either UploadApiError FileBS
expectOneFile multipart = case files multipart of
  [file] -> Right $ FileBS (fdFileName file) (fdPayload file)
  [] -> Left NoExperimentFileError
  _ -> Left TooManyExperimentFilesError

-- | File information
data FileBS = FileBS
  { fbsName :: Text
  , fbsFile :: LByteString
  } deriving stock (Generic, Show)

instance Buildable FileBS where
  build FileBS {..} = "file for parse with name: " +| fbsName |+ ""

instance ToSchema FileBS where
  declareNamedSchema _ = pure $ NamedSchema (Just "ByteString") binarySchema

instance HasSwagger api => HasSwagger (MultipartForm Mem FileBS :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api) & addParam param
    where
      param = mempty
        & name .~ "file"
        & required ?~ True
        & description ?~ "Experiment to parse"
        & schema .~ ParamOther (mempty
            & in_ .~ ParamFormData
            & paramSchema .~ (mempty & type_ ?~ S.SwaggerFile))

instance FromMultipart Mem FileBS where
  fromMultipart = first pretty . expectOneFile

-- | Input data submitted along with uploaded file.
data FileUploadReq = FileUploadReq
  { furProject :: ProjectId
  -- ^ ID of the project the file belongs to.
  , furTestMethodology :: MethodologyId
  -- ^ ID of the test methodology used throughout the file.
  , furDescription :: Maybe Text
  -- ^ Description of the file.
  , furFile :: FileBS
  -- ^ File name with the file
  } deriving stock (Generic, Show)

instance Buildable FileUploadReq where
  build FileUploadReq {furFile = FileBS{..}, ..} =
    "file for upload with name \"" +| fbsName |+ "\" and upload options:" <>
    " projectId: " +| furProject |+
    " methodologyId: " +| furTestMethodology |+
    " description: " +| furDescription |+ ""

instance ToSchema FileUploadReq where
  declareNamedSchema = gDeclareNamedSchema

instance HasSwagger api => HasSwagger (MultipartForm Mem FileUploadReq :> api) where
  toSwagger _ = toSwagger (Proxy :: Proxy api)
    & addParam file
    & addParam project
    & addParam methodology
    & addParam desc
    where
      file = mempty
        & name .~ "file"
        & required ?~ True
        & description ?~ "Experiment to upload"
        & schema .~ ParamOther (mempty
            & in_ .~ ParamFormData
            & paramSchema .~ (mempty & type_ ?~ S.SwaggerFile))
      project = mempty
        & name .~ "projectId"
        & required ?~ True
        & description ?~ "Project id"
        & schema .~ ParamOther (mempty
            & in_ .~ ParamFormData
            & paramSchema .~ (mempty & type_ ?~ S.SwaggerString))
      methodology = mempty
        & name .~ "methodologyId"
        & required ?~ True
        & description ?~ "Methodology id"
        & schema .~ ParamOther (mempty
            & in_ .~ ParamFormData
            & paramSchema .~ (mempty & type_ ?~ S.SwaggerString))
      desc = mempty
        & name .~ "description"
        & required ?~ False
        & description ?~ "Description"
        & schema .~ ParamOther (mempty
            & in_ .~ ParamFormData
            & paramSchema .~ (mempty & type_ ?~ S.SwaggerString))

instance FromMultipart Mem FileUploadReq where
  fromMultipart multipartData = do
    let toId = first toString . (SqlId <$>) . readEither
    furProject <- lookupInput "projectId" multipartData >>= toId
    furTestMethodology <- lookupInput "methodologyId" multipartData >>= toId
    let furDescription = rightToMaybe $ lookupInput "description" multipartData
    furFile <- fromMultipart multipartData
    pure FileUploadReq{..}

-- | Summary of an experiment data file.
newtype FileSummary = FileSummary
  { unFileSummary :: [FileSummaryItem]
  } deriving stock (Generic, Show, Eq)
    deriving newtype (Buildable)

instance Buildable (ForResponseLog FileSummary) where
  build (ForResponseLog (FileSummary items)) =
    buildListForResponse (take 5) (ForResponseLog items)

-- | This type holds name of a compound or target and its ID if this item
-- is already known. For new targets and compounds we can't provide IDs
-- because they are not assigned yet.
data NameAndId what = NameAndId
  { iadName :: Text
  -- ^ Name of the entity.
  , ianId :: Maybe (SqlId what)
  -- ^ ID of the entity if available (entity is already in DB).
  } deriving stock (Generic, Show, Eq, Ord)

instance Buildable (NameAndId what) where
  build (NameAndId nm mId) = maybe nameB (mappend (nameB <> ",") . build) mId
    where
      nameB = build nm

instance Buildable (ForResponseLog (NameAndId what)) where
  build = buildForResponse

-- | One element in 'FileSummary'. Corresponds to one target from the file.
-- Contains all compounds that interact with the target in the file.
-- Also contains information whether this target is new or already known.
data FileSummaryItem = FileSummaryItem
  { fsiTarget :: NameAndId 'TargetId
  -- ^ A target from the file. If it's a new target, its ID is unknown.
  , fsiCompounds :: [NameAndId 'CompoundId]
  -- ^ All compounds interacting with this target.
  } deriving stock (Generic, Show, Eq, Ord)

instance Buildable FileSummaryItem where
  build (FileSummaryItem target compounds) =
    build target <> " interacts with " <> build compounds

instance Buildable (ForResponseLog FileSummaryItem) where
  build = buildForResponse

deriveToJSON ednaAesonWebOptions ''NameAndId
deriveToJSON ednaAesonWebOptions ''FileSummaryItem

deriving newtype instance ToJSON FileSummary

instance ToSchema (NameAndId t) where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema FileSummaryItem where
  declareNamedSchema = gDeclareNamedSchema

deriving newtype instance ToSchema FileSummary
