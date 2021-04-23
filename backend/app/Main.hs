-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Main
  ( main
  ) where

import Universum

import qualified Data.Yaml as Y
import Main.Utf8 (withUtf8)
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Edna.Config.Definition (defaultEdnaConfig)
import Edna.Config.Utils (configPathParser)
import Edna.Setup (runEdna)
import Edna.Web.Server (edna)

main :: IO ()
main = withUtf8 $ do
  hPutStrLn @Text stderr "Edna server is started"
  configPath <- execParser $
    info (helper <*> configPathParser) $
    fullDesc <> progDesc "Edna API server."
  config <- maybe (pure defaultEdnaConfig) Y.decodeFileThrow configPath
  runEdna config edna
