module Test.Edna
  ( unit_stub_is_2
  , spec_db
  ) where

import Universum

import Test.Hspec (Spec, it, shouldBe)
import Test.HUnit (Assertion)

import Edna.Web.Handlers ()
import Test.Setup (withContext)

unit_stub_is_2 :: Assertion
unit_stub_is_2 = pure ()

spec_db :: Spec
spec_db = withContext . it "DB connection" $ \_ -> True `shouldBe` True
