module Test.Edna
  ( unit_stub_is_2
  ) where

import Universum

import Test.HUnit (Assertion, (@?=))

import Stub (stub)

unit_stub_is_2 :: Assertion
unit_stub_is_2 = n - 1 @?= stub
  where
    n = 3
