-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for types from other packages.

module Edna.Orphans () where

import Universum

import RIO (RIO(..))
import Servant.Multipart (MultipartForm')
import Servant.Util.Combinators.Logging (ApiCanLogArg)
import Servant.Util.Common.Common (ApiHasArgClass(..))

-- It's also available in @rio-orphans@, but that would add extra dependencies,
-- so it's simpler to just have one line for now.
deriving newtype instance MonadCatch (RIO env)

----------------
-- Logging
----------------

instance ApiHasArgClass (MultipartForm' mods tag t) where
  type ApiArg (MultipartForm' mods tag t) = t
  apiArgName _ = "multipart"

instance ApiCanLogArg (MultipartForm' mods tag t)
