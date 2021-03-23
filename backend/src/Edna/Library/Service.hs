-- | Implementation of library functionality

module Edna.Library.Service
  ( -- * Implementation
    getTarget
  , getTargets
  , getCompound
  , getCompounds
  , editChemSoft
  ) where

import Universum

import qualified Edna.Library.DB.Query as Q

import Database.Beam.Backend (SqlSerial(..))

import Edna.Library.DB.Schema (CompoundRec, CompoundT(..))
import Edna.Library.Error (LibraryError(..))
import Edna.Library.Web.Types (CompoundResp(..), TargetResp)
import Edna.Setup (Edna)
import Edna.Util (IdType(..), SqlId(..), justOrThrow)
import Edna.Util.URI (parseURI, renderURI)
import Edna.Web.Types (StubSortBy, URI, WithId(..))

-- | Get target with given ID.
getTarget :: SqlId 'TargetId -> Edna (WithId 'TargetId TargetResp)
getTarget targetSqlId =
  Q.getTargetById targetSqlId >>= justOrThrow (LETargetNotFound targetSqlId)

-- | Get all targets with optional pagination and sorting.
-- Pagination and sorting parameters are currently ignored.
getTargets :: Maybe Word -> Maybe Word -> Maybe StubSortBy -> Edna [WithId 'TargetId TargetResp]
getTargets _ _ _ = Q.getTargets

compoundToResp :: CompoundRec -> Edna (WithId 'CompoundId CompoundResp)
compoundToResp CompoundRec{..} = do
  url <- case cChemsoftLink of
    Just link -> Just <$> justOrThrow (LEInvalidURI link) (parseURI link)
    Nothing -> pure Nothing
  pure $ WithId (SqlId $ unSerial cCompoundId) $ CompoundResp
    { crName = cName
    , crChemSoft = url
    , crAdditionDate = cCreationDate
    }

getCompound :: SqlId 'CompoundId -> Edna (WithId 'CompoundId CompoundResp)
getCompound compoundSqlId = Q.getCompoundById compoundSqlId >>=
  justOrThrow (LECompoundNotFound compoundSqlId) >>= compoundToResp

getCompounds
  :: Maybe Word
  -> Maybe Word
  -> Maybe StubSortBy
  -> Edna [WithId 'CompoundId CompoundResp]
getCompounds _ _ _ = Q.getCompounds >>= mapM compoundToResp

editChemSoft :: SqlId 'CompoundId -> URI -> Edna (WithId 'CompoundId CompoundResp)
editChemSoft compoundSqlId uri = do
  compound <- Q.getCompoundById compoundSqlId >>= justOrThrow (LECompoundNotFound compoundSqlId)
  let uriText = renderURI uri
  Q.editCompoundChemSoft compoundSqlId uriText
  compoundToResp compound {cChemsoftLink = Just uriText}
