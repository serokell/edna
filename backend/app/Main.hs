module Main
  ( main
  ) where

import Universum

import qualified Data.Yaml as Y
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)

import Edna.Config.Utils (configPathParser)
import Edna.Config.Definition (defaultEdnaConfig)
import Edna.Setup (runEdna)
import Edna.Web.Server (edna)

main :: IO ()
main = do
  configPath <- execParser $
    info (helper <*> configPathParser) $
    fullDesc <> progDesc "Edna API server."
  config <- maybe (pure defaultEdnaConfig) Y.decodeFileThrow configPath
  runEdna config edna
