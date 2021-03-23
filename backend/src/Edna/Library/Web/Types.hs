module Edna.Library.Web.Types
  ( TargetResp (..)
  , CompoundResp (..)
  ) where

import Universum

import Data.Aeson.TH (deriveJSON)
import Data.Swagger (ToSchema(..))
import Data.Time (LocalTime)
import Network.URI.JSON ()

import Edna.Util (ednaAesonWebOptions, gDeclareNamedSchema)
import Edna.Web.Types (URI)

-- | Targets are not submitted directly by users, so for now
-- there is only one representation for frontend.
data TargetResp = TargetResp
  { trName :: Text
  -- ^ Name of the target.
  , trProjects :: [Text]
  -- ^ Names of all projects where this target is involved.
  , trCreationDate :: LocalTime
  -- ^ Timestamp when the target was created (by uploading a file).
  } deriving stock (Generic, Show)

deriveJSON ednaAesonWebOptions ''TargetResp

instance ToSchema TargetResp where
  declareNamedSchema = gDeclareNamedSchema

-- | Compounds are not submitted directly by users, so for now
-- there is only one representation for frontend.
-- MDe links are trivial to generate, so we offload this task to frontend.
data CompoundResp = CompoundResp
  { crName :: Text
  -- ^ Name of the compound, it may be changed to be a number later.
  , crChemSoft :: Maybe URI
  -- ^ Link to ChemSoft.
  , crAdditionDate :: LocalTime
  -- ^ Timestamp when this compound was added to the system (by uploading a file).
  } deriving stock (Generic, Show)

deriveJSON ednaAesonWebOptions ''CompoundResp

instance ToSchema URI => ToSchema CompoundResp where
  declareNamedSchema = gDeclareNamedSchema
