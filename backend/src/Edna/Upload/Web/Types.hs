-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Edna.Upload.Web.Types
  ( FileSummary (..)
  , NameAndId (..)
  , FileSummaryItem (..)
  , FileBS (..)
  , FileUploadReq (..)

  , sortFileSummary
  ) where

import Universum

import qualified Data.HashMap.Strict.InsOrd as InsMap
import qualified Data.OpenApi as O

import Control.Lens (at, (?~))
import Data.Aeson (ToJSON)
import Data.Aeson.TH (deriveToJSON)
import Data.OpenApi (NamedSchema(..), ToSchema(..), binarySchema)
import Fmt (Buildable(..), pretty, (+|), (|+))
import Network.HTTP.Media (MediaType)
import Servant.API ((:>))
import Servant.Multipart
  (FromMultipart(..), Mem, MultipartData, MultipartForm, fdFileName, fdPayload, files, lookupInput)
import Servant.OpenApi.Internal (HasOpenApi(..), addRequestBody)
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

instance HasOpenApi api => HasOpenApi (MultipartForm Mem FileBS :> api) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy api) & addRequestBody body
    where
      body :: O.RequestBody
      body = mempty
        & O.content .~ content
        & O.required ?~ True

      content :: InsMap.InsOrdHashMap MediaType O.MediaTypeObject
      content = mempty & at "multipart/form-data" ?~ (mempty
        & O.schema ?~ O.Inline (mempty
          & O.type_ ?~ O.OpenApiObject
          & O.required .~ ["file"]
          & O.properties .~ (mempty
            & at "file" ?~ O.Inline (mempty
              & O.description ?~ "Experiment to parse"
              & O.type_ ?~ O.OpenApiString
              & O.format ?~ "binary"
              )
            )
          )
        )

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

instance HasOpenApi api => HasOpenApi (MultipartForm Mem FileUploadReq :> api) where
  toOpenApi _ = toOpenApi (Proxy :: Proxy api) & addRequestBody body
    where
      body :: O.RequestBody
      body = mempty
        & O.content .~ content
        & O.required ?~ True

      content :: InsMap.InsOrdHashMap MediaType O.MediaTypeObject
      content = mempty & at "multipart/form-data" ?~ (mempty
        & O.schema ?~ O.Inline (mempty
          & O.type_ ?~ O.OpenApiObject
          & O.required .~ ["file", "projectId", "methodologyId"]
          & O.properties .~ (mempty
            & at "file" ?~ O.Inline (mempty
              & O.description ?~ "Experiment to upload"
              & O.type_ ?~ O.OpenApiString
              & O.format ?~ "binary"
              )
            & at "projectId" ?~ O.Inline (mempty
              & O.description ?~ "Project id"
              & O.type_ ?~ O.OpenApiInteger
              & O.format ?~ "int32"
              )
            & at "methodologyId" ?~ O.Inline (mempty
              & O.description ?~ "Methodology id"
              & O.type_ ?~ O.OpenApiInteger
              & O.format ?~ "int32"
              )
            & at "description" ?~ O.Inline (mempty
              & O.description ?~ "Description"
              & O.type_ ?~ O.OpenApiString
              )
            )
          )
        )

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

-- | Deeply sort all items (targets and compounds for each target) in 'FileSummary'.
sortFileSummary :: FileSummary -> FileSummary
sortFileSummary (FileSummary items) = FileSummary $ sortOn fsiTarget items'
  where
    items' = map sortCompounds items
    sortCompounds item = item { fsiCompounds = sort (fsiCompounds item)}

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
