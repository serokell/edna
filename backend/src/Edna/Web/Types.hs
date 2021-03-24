-- | Bridge types used to communicate between the server app and frontend.

module Edna.Web.Types
  ( WithId (..)
  , WithExtra (..)
  , StubSortBy (..)
  , FileSummary (..)
  , NameAndId (..)
  , FileSummaryItem (..)
  , Project (..)
  , ProjectExtra (..)

  -- * Re-exported for convenience
  , URI (..)
  ) where

import Universum

import Data.Aeson (ToJSON)
import Data.Aeson.TH (deriveJSON, deriveToJSON)
import Data.Swagger (SwaggerType(..), ToParamSchema(..), ToSchema(..), enum_, type_)
import Lens.Micro ((?~))
import Network.URI (URI(..))
import Network.URI.JSON ()
import Servant (FromHttpApiData(..))

import Edna.Util (IdType(..), SqlId(..), ednaAesonWebOptions, gDeclareNamedSchema)

----------------
-- General types
----------------

-- | This data type is useful when you want to return something with its ID.
data WithId k t = WithId
  { wiId :: SqlId k
  , wItem :: t
  } deriving stock (Generic, Show)

-- | This data type is used when you want to return something with its ID and
-- some additional data that was not submitted by end users, but is maintained
-- by the application.
data WithExtra k t e = WithExtra
  { weId :: SqlId k
  , weItem :: t
  , weExtra :: e
  } deriving stock (Generic, Show)

-- | A stub to specify the sorting order, most likely will be replaced with
-- @servant-util@.
data StubSortBy =
    SortByName
  | SortBySomething

instance FromHttpApiData StubSortBy where
  parseQueryParam = \case
    "name" -> pure SortByName
    "something" -> pure SortBySomething
    x -> Left $ "unknown sorting order: " <> x

----------------
-- Entities
----------------

-- | Summary of an experiment data file.
newtype FileSummary = FileSummary
  { unFileSummary :: [FileSummaryItem]
  } deriving stock (Generic, Show, Eq)

-- | This type holds name of a compound or target and its ID if this item
-- is already known. For new targets and compounds we can't provide IDs
-- because they are not assigned yet.
data NameAndId what = NameAndId
  { iadName :: Text
  -- ^ Name of the entity.
  , ianId :: Maybe (SqlId what)
  -- ^ ID of the entity if available (entity is already in DB).
  } deriving stock (Generic, Show, Eq, Ord)

-- | One element in 'FileSummary'. Corresponds to one target from the file.
-- Contains all compounds that interact with the target in the file.
-- Also contains information whether this target is new or already known.
data FileSummaryItem = FileSummaryItem
  { fsiTarget :: NameAndId 'TargetId
  -- ^ A target from the file. If it's a new target, its ID is unknown.
  , fsiCompounds :: [NameAndId 'CompoundId]
  -- ^ All compounds interacting with this target.
  } deriving stock (Generic, Show, Eq, Ord)

-- | Project as submitted by end users.
data Project = Project
  { pName :: Text
  , pDescription :: Text
  } deriving stock (Generic, Show, Eq)

-- | Extra data about projects that is not submitted by users, but is stored
-- in DB.
data ProjectExtra = ProjectExtra
  { peCreationDate :: Word64
  -- ^ Unsigned number, UNIX timestamp.
  -- Can be changed to a more specific type later.
  -- The same applies to other timestamps.
  , peLastUpdate :: Word64
  , peCompoundNames :: [Text]
  -- ^ Names of all compounds involved in this project.
  } deriving stock (Generic, Show)

----------------
-- JSON
----------------

deriveToJSON ednaAesonWebOptions ''WithId
deriveToJSON ednaAesonWebOptions ''WithExtra
deriveToJSON ednaAesonWebOptions ''NameAndId
deriveToJSON ednaAesonWebOptions ''FileSummaryItem
deriveJSON ednaAesonWebOptions ''Project
deriveJSON ednaAesonWebOptions ''ProjectExtra

deriving newtype instance ToJSON FileSummary

----------------
-- Swagger
----------------

instance ToSchema t => ToSchema (WithId k t) where
  declareNamedSchema = gDeclareNamedSchema

instance (ToSchema t, ToSchema e) => ToSchema (WithExtra k t e) where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema (NameAndId t) where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema FileSummaryItem where
  declareNamedSchema = gDeclareNamedSchema

deriving newtype instance ToSchema FileSummary

instance ToSchema Project where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema ProjectExtra where
  declareNamedSchema = gDeclareNamedSchema

instance ToParamSchema StubSortBy where
  toParamSchema _ = mempty
     & type_ ?~ SwaggerString
     & enum_ ?~ [ "name", "something" ]
