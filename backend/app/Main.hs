-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Main
  ( main
  ) where

import Universum

import Main.Utf8 (withUtf8)
import Options.Applicative (execParser)

import Edna.Config.CLA (EdnaOptions(eoDumpConfig), ednaOpts)
import Edna.Config.Prepare (prepareConfig)
import Edna.Setup (dumpConfig, runEdna)
import Edna.Web.Server (edna)
import Edna.Logging (logUnconditionally)

main :: IO ()
main = withUtf8 $ do
  options <- execParser ednaOpts
  logUnconditionally "Edna server is started"
  config <- prepareConfig options
  runEdna config edna
