-- | Bridge types used to communicate between the server app and frontend.

module Edna.Web.Types
  ( ExperimentalMeasurement (..)
  , SqlId (..)
  , WithId (..)
  , WithExtra (..)
  , StubSortBy (..)
  , Project (..)
  , ProjectExtra (..)
  , TestMethodology (..)
  , Compound (..)

  -- * Re-exported for convenience
  , URI
  ) where

import Universum

import Data.Aeson (ToJSON)
import Data.Aeson.TH (deriveJSON, deriveToJSON)
import Data.Swagger (SwaggerType(..), ToParamSchema(..), ToSchema(..), enum_, type_)
import Lens.Micro ((?~))
import Network.URI (URI)
import Network.URI.JSON ()
import Servant (FromHttpApiData(..))

import Edna.Util (ednaAesonWebOptions, gDeclareNamedSchema, gToParamSchema)

----------------
-- Legacy
----------------

data ExperimentalMeasurement = ExperimentalMeasurement
  { emCompoundId :: Text
  , emTargetId :: Text
  , emConcentration :: Double
  , emSignal :: Double
  , emOutlier :: Bool
  } deriving stock (Generic, Show, Eq)

----------------
-- General types
----------------

-- | A simple wrapper over 'Word'. At the data layer, we identify all entities
-- with numeric IDs and this data type corresponds to such an ID.
-- It has a phantom parameter type which the type of the object
-- identified by this ID.
newtype SqlId t = SqlId
  { unSqlId :: Word
  } deriving stock (Generic)
    deriving newtype (FromHttpApiData, ToJSON, ToSchema)

-- | This data type is useful when you want to return something with its ID.
data WithId t = WithId
  { wiId :: SqlId t
  , wItem :: t
  } deriving stock (Generic)

-- | This data type is used when you want to return something with its ID and
-- some additional data that was not submitted by end users, but is maintained
-- by the application.
data WithExtra t e = WithExtra
  { weId :: SqlId t
  , weItem :: t
  , weExtra :: e
  } deriving stock (Generic)

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

-- | Project as submitted by end users.
data Project = Project
  { pName :: Text
  , pDescription :: Text
  } deriving stock (Generic)

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
  } deriving stock (Generic)

-- | Test methodology as submitted by end users.
data TestMethodology = TestMethodology
  { tmName :: Text
  , tmDescription :: Text
  , tmConfluence :: URI
  } deriving stock (Generic)

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
  } deriving stock (Generic)

----------------
-- JSON
----------------

deriveToJSON ednaAesonWebOptions ''ExperimentalMeasurement
deriveToJSON ednaAesonWebOptions ''WithId
deriveToJSON ednaAesonWebOptions ''WithExtra
deriveJSON ednaAesonWebOptions ''Project
deriveJSON ednaAesonWebOptions ''ProjectExtra
deriveJSON ednaAesonWebOptions ''TestMethodology
deriveJSON ednaAesonWebOptions ''Compound

----------------
-- Swagger
----------------

instance ToSchema ExperimentalMeasurement where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema t => ToSchema (WithId t) where
  declareNamedSchema = gDeclareNamedSchema

instance (ToSchema t, ToSchema e) => ToSchema (WithExtra t e) where
  declareNamedSchema = gDeclareNamedSchema

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

instance ToParamSchema (SqlId t) where
  toParamSchema = gToParamSchema

instance ToParamSchema StubSortBy where
  toParamSchema _ = mempty
     & type_ ?~ SwaggerString
     & enum_ ?~ [ "name", "something" ]
