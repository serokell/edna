{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for types from other packages.

module Edna.Orphans () where

import Universum

import Fmt (Buildable(..), (+|), (|+))
import RIO (RIO(..))
import Servant.Multipart (MultipartData(..), MultipartForm')
import Servant.Util.Combinators.Logging (ApiCanLogArg)
import Servant.Util.Common.Common (ApiHasArgClass(..))

-- It's also available in @rio-orphans@, but that would add extra dependencies,
-- so it's simpler to just have one line for now.
deriving newtype instance MonadCatch (RIO env)

----------------
-- Logging
----------------

instance ApiHasArgClass (MultipartForm' mods tag (MultipartData tag)) where
  type ApiArg (MultipartForm' mods tag (MultipartData tag)) = MultipartData tag
  apiArgName _ = "multipart"

instance ApiCanLogArg (MultipartForm' mods tag (MultipartData tag))

instance Buildable (MultipartData tag) where
  build MultipartData {..} =
    "multipart with " +| length inputs |+
    " inputs and " +| length files |+ " files"
