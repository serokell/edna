-- | Bridge types used to communicate between the server app and frontend.

module Edna.Web.Types
  ( SqlId (..)
  , WithId (..)
  , WithExtra (..)
  , StubSortBy (..)
  , FileSummary (..)
  , FileSummaryItem (..)
  , Project (..)
  , ProjectExtra (..)
  , TestMethodology (..)
  , Compound (..)
  , Target (..)

  -- * Re-exported for convenience
  , URI (..)
  ) where

import Universum

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.TH (deriveJSON, deriveToJSON)
import Data.Swagger (SwaggerType(..), ToParamSchema(..), ToSchema(..), enum_, type_)
import Fmt (Buildable(..))
import Lens.Micro ((?~))
import Network.URI (URI(..))
import Network.URI.JSON ()
import Servant (FromHttpApiData(..))

import Edna.Util (ednaAesonWebOptions, gDeclareNamedSchema, gToParamSchema)

----------------
-- General types
----------------

-- | A simple wrapper over 'Word'. At the data layer, we identify all entities
-- with numeric IDs and this data type corresponds to such an ID.
-- It has a phantom parameter type which the type of the object
-- identified by this ID.
newtype SqlId t = SqlId
  { unSqlId :: Word
  } deriving stock (Generic, Show, Eq)
    deriving newtype (FromHttpApiData, FromJSON, ToJSON, ToSchema, Hashable)

instance Buildable (SqlId t) where
  build (SqlId n) = "ID#" <> build n

-- | This data type is useful when you want to return something with its ID.
data WithId t = WithId
  { wiId :: SqlId t
  , wItem :: t
  } deriving stock (Generic, Show)

-- | This data type is used when you want to return something with its ID and
-- some additional data that was not submitted by end users, but is maintained
-- by the application.
data WithExtra t e = WithExtra
  { weId :: SqlId t
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

-- | One element in 'FileSummary'. Corresponds to one target from the file.
-- Contains all compounds that interact with the target in the file.
-- Also contains information whether this target is new or already known.
data FileSummaryItem = FileSummaryItem
  { fsiTarget :: Either (SqlId Target) Text
  -- ID of a target from the file. If it's a new target, its name is returned
  -- instead.
  , fsiCompounds :: [Either (SqlId Compound) Text]
  -- IDs of all compounds interacting with this target. Or names for new ones.
  } deriving stock (Generic, Show, Eq)

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

-- | Test methodology as submitted by end users.
data TestMethodology = TestMethodology
  { tmName :: Text
  , tmDescription :: Text
  , tmConfluence :: URI
  } deriving stock (Generic, Show, Eq)

-- | Compounds are not submitted directly by users, so for now
-- there is only one representation for frontend.
-- MDe links are trivial to generate, so we offload this task to frontend.
data Compound = Compound
  { cName :: Text
  -- ^ Name of the compound, it may be changed to be a number later.
  , cChemSoft :: Maybe URI
  -- ^ Link to ChemSoft.
  , cAdditionDate :: Word64
  -- ^ UNIX timestamp when this compound was added to the system.
  } deriving stock (Generic, Show)

-- | Targets are not submitted directly by users, so for now
-- there is only one representation for frontend.
data Target = Target
  { tName :: Text
  -- ^ Name of the target.
  , tProjects :: [Text]
  -- ^ Names of all projects where this target is involved.
  , tCreationDate :: Word64
  -- ^ UNIX timestamp when the target was created.
  } deriving stock (Generic, Show)

----------------
-- JSON
----------------

deriveToJSON ednaAesonWebOptions ''WithId
deriveToJSON ednaAesonWebOptions ''WithExtra
deriveToJSON ednaAesonWebOptions ''FileSummaryItem
deriveJSON ednaAesonWebOptions ''Project
deriveJSON ednaAesonWebOptions ''ProjectExtra
deriveJSON ednaAesonWebOptions ''TestMethodology
deriveJSON ednaAesonWebOptions ''Compound
deriveJSON ednaAesonWebOptions ''Target

deriving newtype instance ToJSON FileSummary

----------------
-- Swagger
----------------

instance ToSchema t => ToSchema (WithId t) where
  declareNamedSchema = gDeclareNamedSchema

instance (ToSchema t, ToSchema e) => ToSchema (WithExtra t e) where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema FileSummaryItem where
  declareNamedSchema = gDeclareNamedSchema

deriving newtype instance ToSchema FileSummary

instance ToSchema Project where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema ProjectExtra where
  declareNamedSchema = gDeclareNamedSchema

-- We define @ToSchema URI@ elsewhere to have less modules
-- with orphans.
instance ToSchema URI => ToSchema TestMethodology where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema URI => ToSchema Compound where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema Target where
  declareNamedSchema = gDeclareNamedSchema

instance ToParamSchema (SqlId t) where
  toParamSchema = gToParamSchema

instance ToParamSchema StubSortBy where
  toParamSchema _ = mempty
     & type_ ?~ SwaggerString
     & enum_ ?~ [ "name", "something" ]
