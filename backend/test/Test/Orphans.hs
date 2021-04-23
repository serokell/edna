-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for types from other packages
-- needed only in tests.

module Test.Orphans () where

import Universum

import RIO (RIO(..))

-- It's better to use @throw@ family of functions instead of @fail@, but
-- in tests we want to do incomplete pattern-matching sometimes and @MonadFail@
-- is needed for that.
instance MonadFail (RIO env) where
  fail = liftIO . fail
