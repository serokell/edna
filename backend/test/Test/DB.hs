module Test.DB
  ( spec_db
  ) where

import Universum

import Test.Hspec (Spec, it, shouldBe)

import Test.Setup (withContext)

spec_db :: Spec
spec_db = withContext . it "DB connection" $ \_ -> True `shouldBe` True
