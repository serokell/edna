module Test.DBSpec
  ( spec
  ) where

import Universum

import Test.Hspec (Spec, it, shouldBe)

import Test.Setup (withContext)

spec :: Spec
spec = withContext . it "DB connection" $ \_ -> True `shouldBe` True
