{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for types from other packages.

module Edna.Orphans () where

import Universum

import RIO (RIO(..))

-- It's also available in @rio-orphans@, but that would add extra dependencies,
-- so it's simpler to just have one line for now.
deriving newtype instance MonadCatch (RIO env)
