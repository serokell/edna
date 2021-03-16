module Test.SwaggerSpec
  ( spec
  ) where

import Universum

import Servant.Swagger (validateEveryToJSON)
import Test.Hspec (Spec, context)

import Edna.Web.API (ednaAPI)
import Edna.Web.Swagger ()

import Test.Gen ()

spec :: Spec
spec = do
  context "ToJSON matches ToSchema" $ validateEveryToJSON ednaAPI
