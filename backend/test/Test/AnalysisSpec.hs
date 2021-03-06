-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Unit tests for Analysis modules.

module Test.AnalysisSpec
  ( spec
  ) where

import Universum

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Edna.Analysis.FourPL
import Edna.ExperimentReader.Types (measurementToPair)
import Edna.Util (SqlId(..))

import Test.Orphans ()
import Test.SampleData (autoOutlierMeasurements)
import Test.Setup (runTestEdna, specWithContext)

spec :: Spec
spec = specWithContext do
  describe "analyse4PL" $ do
    it "returns empty result for empty request" $ runTestEdna $
      analyse4PL [] >>= liftIO . (`shouldBe` [])
    it "returns an error for empty list of points" $ runTestEdna $ do
      [(expId, resp)] <- analyse4PL [req []]
      liftIO $ do
        expId `shouldBe` experimentId
        resp `shouldSatisfy` isLeft
    it "returns a reasonable result for sample data" $ runTestEdna $ do
      let points = map measurementToPair autoOutlierMeasurements
      [(expId, Right resp)] <- analyse4PL [req points]
      liftIO $ do
        expId `shouldBe` experimentId
        plrspNewSubExp resp `shouldSatisfy` isJust
  where
    experimentId = SqlId 1
    req points = Params4PLReq
      { plreqExperiment = experimentId
      , plreqFindOutliers = True
      , plreqData = points
      }
