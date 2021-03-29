{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utilities to work with 'URI'.

module Edna.Util.URI
  ( renderURI
  , parseURI
  ) where

import Universum

import Fmt (Buildable(..))
import Network.URI (URI)
import qualified Network.URI as URI

-- | Render 'URI' in the way we use to communicate with outer world.
renderURI :: URI -> Text
renderURI = show

instance Buildable URI where
  build = build . renderURI

-- | Parse 'URI' from the format we use to communicate with outer world.
--
-- Currently parsing logic is very permissive and accepts absolute and relative
-- URIs with optional fragment identifier.
-- So even empty URI is permitted.
-- We can make it stricter later if we want.
parseURI :: Text -> Maybe URI
parseURI = URI.parseURIReference . toString
