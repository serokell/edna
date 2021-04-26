-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Tests for @Edna.Util.URI@.

module Test.Util.URISpec
  ( spec
  ) where

import Universum

import qualified Data.Aeson as Aeson
import Hedgehog (forAll, test, tripping)
import Network.URI (URI, nullURI)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.Hedgehog (hedgehog)

import Edna.Util.URI (parseURI, renderURI)

import Test.Gen (genURI)

spec :: Spec
spec = do
  describe "parseURI" $ do
    it "accepts empty URI" $
      parseURI "" `shouldBe` Just nullURI
    it "accepts relative URI" $ do
      parseURI "foo.bar" `shouldSatisfy` isJust
      parseURI "foo" `shouldSatisfy` isJust
    it "is reverse for 'renderURI'" $ hedgehog $ do
      uri <- forAll genURI
      test $ tripping uri renderURI parseURI
  describe "JSON parsing" $ do
    it "accepts empty URI" $
      Aeson.decode "\"\"" `shouldBe` Just nullURI
    it "accepts relative URI" $ do
      Aeson.decode @URI "\"foo.bar\"" `shouldSatisfy` isJust
      Aeson.decode @URI "\"foo\"" `shouldSatisfy` isJust
