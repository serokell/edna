module Edna.Upload.Web.Types
  ( FileSummary (..)
  , NameAndId (..)
  , FileSummaryItem (..)
  ) where

import Universum

import Data.Aeson (ToJSON)
import Data.Aeson.TH (deriveToJSON)
import Data.Swagger (ToSchema(..))
import Fmt (Buildable(..))
import Servant.Util.Combinators.Logging (ForResponseLog(..), buildForResponse, buildListForResponse)

import Edna.Util (IdType(..), SqlId, ednaAesonWebOptions, gDeclareNamedSchema)

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
  build (NameAndId name mId) = maybe nameB (mappend (nameB <> ",") . build) mId
    where
      nameB = build name

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
