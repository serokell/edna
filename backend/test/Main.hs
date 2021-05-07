-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Main
  ( main
  ) where

import Universum

import qualified Spec

import Test.Hspec.Core.Runner (evaluateSummary, runSpec)

import Test.API.Library as API

import Test.Setup (hspecConfig)

main :: IO ()
main = do
  unitSummary <- runSpec Spec.spec hspecConfig
  integrationSummary <- API.spec hspecConfig
  evaluateSummary $ mconcat [unitSummary, integrationSummary]
