module Test.Swagger
  ( spec_swagger
  ) where

import Universum

import Servant.Swagger (validateEveryToJSON)
import Test.Hspec (Spec, context)

import Edna.Web.API (ednaAPI)
import Edna.Web.Swagger ()

import Test.Gen ()

spec_swagger :: Spec
spec_swagger = do
  context "ToJSON matches ToSchema" $ validateEveryToJSON ednaAPI
