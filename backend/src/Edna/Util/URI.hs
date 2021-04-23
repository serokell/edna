-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Utilities to work with 'URI'.

module Edna.Util.URI
  ( URI
  , renderURI
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
