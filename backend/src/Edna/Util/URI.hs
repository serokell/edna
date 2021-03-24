-- | Utilities to work with 'URI'.

module Edna.Util.URI
  ( renderURI
  , parseURI
  ) where

import Universum

import Network.URI (URI)
import qualified Network.URI as URI

-- | Render 'URI' in the way we use to communicate with outer world.
renderURI :: URI -> Text
renderURI = show

-- | Parse 'URI' from the format we use to communicate with outer world.
--
-- TODO: is 'URI.parseURI' the right function? There are other parsing functions.
parseURI :: Text -> Maybe URI
parseURI = URI.parseURI . toString
