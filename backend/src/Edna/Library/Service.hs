-- | Implementation of library functionality

module Edna.Library.Service
  ( -- * Implementation
    getTarget
  , getTargets

  -- * Types
  , TargetError (..)
  ) where

import Universum

import qualified Edna.Library.DB.Query as Q

import Edna.Library.Error (TargetError(..))
import Edna.Library.Web.Types (TargetResp)
import Edna.Setup (Edna)
import Edna.Util (SqlId(..), justOrThrow, IdType(..))
import Edna.Web.Types (StubSortBy, WithId(..))

-- | Get target with given ID.
getTarget :: SqlId 'TargetId -> Edna (WithId 'TargetId TargetResp)
getTarget targetSqlId = Q.getTargetById targetSqlId >>= justOrThrow (TETargetNotFound targetSqlId)

-- | Get all targets with optional pagination and sorting.
-- Pagination and sorting parameters are currently ignored.
getTargets :: Maybe Word -> Maybe Word -> Maybe StubSortBy -> Edna [WithId 'TargetId TargetResp]
getTargets _ _ _ = Q.getTargets
