-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for types from other packages
-- needed only in tests.

module Test.Orphans () where

import Universum

import Data.Aeson.TH (deriveFromJSON)
import RIO (RIO(..))
import Servant (ToHttpApiData(..), (:>))
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

deriveFromJSON ednaAesonWebOptions ''WithId


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
