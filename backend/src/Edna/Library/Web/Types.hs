-- | Types that exist specifially for Library API.

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
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Fmt (Buildable(..), genericF, (+|), (|+))
import Network.URI.JSON ()
import Servant.Util.Combinators.Logging (ForResponseLog(..), buildForResponse)

import Edna.Util (ednaAesonWebOptions, gDeclareNamedSchema)
import Edna.Util.URI ()
import Edna.Web.Types (URI)

-- | Project as submitted by end users.
data ProjectReq = ProjectReq
  { prqName :: Text
  , prqDescription :: Maybe Text
  } deriving stock (Generic, Show, Eq)

instance Buildable ProjectReq where
  build ProjectReq {..} =
    "Project " +| prqName |+
    " (" +| maybe "no description" build prqDescription |+ ")"

instance Buildable (ForResponseLog ProjectReq) where
  build = buildForResponse

-- | Project as response from server
data ProjectResp = ProjectResp
  { prName :: Text
  , prDescription :: Maybe Text
  , prCreationDate :: UTCTime
  , prLastUpdate :: UTCTime
  , prCompoundNames :: [Text]
  -- ^ Names of all compounds involved in this project.
  } deriving stock (Generic, Show)

instance Buildable ProjectResp where
  build ProjectResp {..} =
    ProjectReq prName prDescription |+
    " created at " +| iso8601Show prCreationDate |+ ""

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

instance Buildable MethodologyReqResp where
  build = genericF

instance Buildable (ForResponseLog MethodologyReqResp) where
  build = buildForResponse

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
  , trAdditionDate :: UTCTime
  -- ^ Timestamp when this target was added to the system (by uploading a file).
  } deriving stock (Generic, Show)

instance Buildable TargetResp where
  build TargetResp {..} =
    "Target " +| trName |+ " with " +| length trProjects |+
    " projects, added on " +| iso8601Show trAdditionDate |+ ""

instance Buildable (ForResponseLog TargetResp) where
  build = buildForResponse

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
  , crAdditionDate :: UTCTime
  -- ^ Timestamp when this compound was added to the system (by uploading a file).
  } deriving stock (Generic, Show)

instance Buildable CompoundResp where
  build = genericF

instance Buildable (ForResponseLog CompoundResp) where
  build = buildForResponse

deriveJSON ednaAesonWebOptions ''CompoundResp

instance ToSchema URI => ToSchema CompoundResp where
  declareNamedSchema = gDeclareNamedSchema
