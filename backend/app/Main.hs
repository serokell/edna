module Main
  ( main
  ) where

import Universum

import qualified Data.Yaml as Y
import Options.Applicative (execParser, fullDesc, helper, info, progDesc)
import qualified Options.Applicative as Opt

import Edna.Config (EdnaConfig)
import Edna.Web.Server (runEdna)

-- | CLI parser for config path.
configPathParser :: Opt.Parser FilePath
configPathParser = Opt.strOption $
  Opt.short 'c' <>
  Opt.long "config" <>
  Opt.metavar "FILEPATH" <>
  Opt.help "Path to configuration file."


main :: IO ()
main = do
  configPath <- execParser $
    info (helper <*> configPathParser) $
    fullDesc <> progDesc "Edna API server."

  Y.decodeFileEither @EdnaConfig configPath >>= \case
    Left e -> putText (show e)
    Right c -> runEdna c
