module Edna.Library.Web.Types
  ( TargetResp (..)
  ) where

import Universum

import Data.Aeson.TH (deriveJSON)
import Data.Swagger (ToSchema(..))
import Data.Time (LocalTime)
import Network.URI.JSON ()

import Edna.Util (ednaAesonWebOptions, gDeclareNamedSchema)

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
