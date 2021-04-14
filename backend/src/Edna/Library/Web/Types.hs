-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Types that exist specifially for Library API.

module Edna.Library.Web.Types
  ( TargetResp (..)
  , TargetWithId
  , TargetPaginationFields
  , CompoundResp (..)
  , CompoundWithId
  , CompoundPaginationFields
  , MethodologyReq (..)
  , MethodologyResp (..)
  , MethodologyWithId
  , MethodologyPaginationFields
  , ProjectReq (..)
  , ProjectResp (..)
  , ProjectWithId
  , ProjectPaginationFields
  ) where

import Universum

import Data.Aeson.TH (deriveJSON)
import Data.Swagger (ToSchema(..))
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Fmt (Buildable(..), genericF, (+|), (|+))
import Network.URI.JSON ()
import Servant.Pagination (HasPagination(..))
import Servant.Util.Combinators.Logging (ForResponseLog(..), buildForResponse)

import Edna.Util (IdType(..), ednaAesonWebOptions, gDeclareNamedSchema)
import Edna.Util.URI ()
import Edna.Web.Types (URI, WithId(..))

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
  } deriving stock (Generic, Show, Eq)

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

type ProjectWithId = WithId 'ProjectId ProjectResp

instance HasPagination ProjectWithId "name" where
  type RangeType ProjectWithId "name" = Text
  getFieldValue _ = prName . wItem

instance HasPagination ProjectWithId "creationDate" where
  type RangeType ProjectWithId "creationDate" = UTCTime
  getFieldValue _ = prCreationDate . wItem

instance HasPagination ProjectWithId "lastUpdate" where
  type RangeType ProjectWithId "lastUpdate" = UTCTime
  getFieldValue _ = prLastUpdate . wItem

type ProjectPaginationFields = '["name", "creationDate", "lastUpdate"]

-- | Test methodology as submitted by end users.
data MethodologyReq = MethodologyReq
  { mrqName :: Text
  , mrqDescription :: Maybe Text
  , mrqConfluence :: Maybe URI
  } deriving stock (Generic, Show, Eq)

instance Buildable MethodologyReq where
  build = genericF

instance Buildable (ForResponseLog MethodologyReq) where
  build = buildForResponse

deriveJSON ednaAesonWebOptions ''MethodologyReq

-- We define @ToSchema URI@ elsewhere to have less modules
-- with orphans.
instance ToSchema URI => ToSchema MethodologyReq where
  declareNamedSchema = gDeclareNamedSchema

-- | Test methodology as response from the server.
data MethodologyResp = MethodologyResp
  { mrName :: Text
  , mrDescription :: Maybe Text
  , mrConfluence :: Maybe URI
  , mrProjects :: [Text]
  } deriving stock (Generic, Show, Eq)

instance Buildable MethodologyResp where
  build = genericF

instance Buildable (ForResponseLog MethodologyResp) where
  build = buildForResponse

deriveJSON ednaAesonWebOptions ''MethodologyResp

instance ToSchema URI => ToSchema MethodologyResp where
  declareNamedSchema = gDeclareNamedSchema

type MethodologyWithId = WithId 'MethodologyId MethodologyResp

instance HasPagination MethodologyWithId "name" where
  type RangeType MethodologyWithId "name" = Text
  getFieldValue _ = mrName . wItem

type MethodologyPaginationFields = '["name"]

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

type TargetWithId = WithId 'TargetId TargetResp

instance HasPagination TargetWithId "name" where
  type RangeType TargetWithId "name" = Text
  getFieldValue _ = trName . wItem

instance HasPagination TargetWithId "additionDate" where
  type RangeType TargetWithId "additionDate" = UTCTime
  getFieldValue _ = trAdditionDate . wItem

type TargetPaginationFields = '["name", "additionDate"]

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
  } deriving stock (Generic, Show, Eq)

instance Buildable CompoundResp where
  build = genericF

instance Buildable (ForResponseLog CompoundResp) where
  build = buildForResponse

deriveJSON ednaAesonWebOptions ''CompoundResp

instance ToSchema URI => ToSchema CompoundResp where
  declareNamedSchema = gDeclareNamedSchema

type CompoundWithId = WithId 'CompoundId CompoundResp

instance HasPagination CompoundWithId "name" where
  type RangeType CompoundWithId "name" = Text
  getFieldValue _ = crName . wItem

instance HasPagination CompoundWithId "additionDate" where
  type RangeType CompoundWithId "additionDate" = UTCTime
  getFieldValue _ = crAdditionDate . wItem

type CompoundPaginationFields = '["name", "additionDate"]
