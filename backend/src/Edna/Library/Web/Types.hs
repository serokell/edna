module Edna.Library.Web.Types
  ( TargetResp (..)
  , CompoundResp (..)
  , MethodologyReqResp (..)
  , ProjectReq (..)
  , ProjectResp (..)
  ) where

import Universum

import Data.Aeson.TH (deriveJSON)
import Data.Swagger (ToSchema(..))
import Data.Time (LocalTime)
import Network.URI.JSON ()

import Edna.Util (ednaAesonWebOptions, gDeclareNamedSchema)
import Edna.Web.Types (URI)

-- | Project as submitted by end users.
data ProjectReq = ProjectReq
  { prqName :: Text
  , prqDescription :: Maybe Text
  } deriving stock (Generic, Show, Eq)

-- | Project as response from server
data ProjectResp = ProjectResp
  { prName :: Text
  , prDescription :: Maybe Text
  , prCreationDate :: LocalTime
  , prLastUpdate :: LocalTime
  , prCompoundNames :: [Text]
  -- ^ Names of all compounds involved in this project.
  } deriving stock (Generic, Show)

deriveJSON ednaAesonWebOptions ''ProjectReq
deriveJSON ednaAesonWebOptions ''ProjectResp

instance ToSchema ProjectReq where
  declareNamedSchema = gDeclareNamedSchema

instance ToSchema ProjectResp where
  declareNamedSchema = gDeclareNamedSchema

-- | Test methodology as submitted by end users.
data MethodologyReqResp = MethodologyReqResp
  { mrpName :: Text
  , mrpDescription :: Maybe Text
  , mrpConfluence :: Maybe URI
  } deriving stock (Generic, Show, Eq)

deriveJSON ednaAesonWebOptions ''MethodologyReqResp

-- We define @ToSchema URI@ elsewhere to have less modules
-- with orphans.
instance ToSchema URI => ToSchema MethodologyReqResp where
  declareNamedSchema = gDeclareNamedSchema

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
