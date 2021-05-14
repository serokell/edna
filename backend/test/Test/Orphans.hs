-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for types from other packages
-- needed only in tests.

module Test.Orphans () where

import Universum

import Data.Aeson.TH (deriveFromJSON)
import Edna.Dashboard.Web.Types
  (ExperimentFileBlob(..), ExperimentMetadata(..), ExperimentResp(..), ExperimentsResp(..),
  MeasurementResp(..), SubExperimentResp(..))
import RIO (RIO(..))
import Servant (ToHttpApiData(..), (:>))
import Servant.API.ContentTypes (MimeUnrender, OctetStream)
import Servant.Multipart (Mem, MultipartForm)
import Servant.QuickCheck.Internal (HasGenRequest(..))
import Servant.Util (PaginationParams, SortingParams)

import Edna.Util (IdType, SqlId(..), ednaAesonWebOptions)
import Edna.Web.Types (WithId)

-- It's better to use @throw@ family of functions instead of @fail@, but
-- in tests we want to do incomplete pattern-matching sometimes and @MonadFail@
-- is needed for that.
instance MonadFail (RIO env) where
  fail = liftIO . fail


deriving newtype instance ToHttpApiData (SqlId (t :: IdType))
deriving newtype instance MimeUnrender OctetStream ExperimentFileBlob

deriveFromJSON ednaAesonWebOptions ''WithId
deriveFromJSON ednaAesonWebOptions ''ExperimentsResp
deriveFromJSON ednaAesonWebOptions ''ExperimentResp
deriveFromJSON ednaAesonWebOptions ''SubExperimentResp
deriveFromJSON ednaAesonWebOptions ''MeasurementResp
deriveFromJSON ednaAesonWebOptions ''ExperimentMetadata


--
-- Instances to allow use of `servant-quickcheck`.
--
instance HasGenRequest sub
  => HasGenRequest (MultipartForm Mem arg :> sub)
  where
    genRequest _ = genRequest (Proxy :: Proxy sub)

instance HasGenRequest sub
  => HasGenRequest (SortingParams args :> PaginationParams :> sub)
  where
    genRequest _ = genRequest (Proxy :: Proxy sub)
