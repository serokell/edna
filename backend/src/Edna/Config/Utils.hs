module Edna.Config.Utils
  ( configPathParser
  , fromConfig
  ) where

import Universum

import qualified Options.Applicative as Opt

import Edna.Config.Definition (EdnaConfig)
import Edna.Setup (Edna, edConfig)
import Lens.Micro.Platform (Getting)

configPathParser :: Opt.Parser (Maybe FilePath)
configPathParser = Opt.optional $ Opt.strOption $
  Opt.short 'c' <>
  Opt.long "config" <>
  Opt.metavar "FILEPATH" <>
  Opt.help "Path to configuration file."

fromConfig :: Getting a EdnaConfig a -> Edna a
fromConfig getter = view (edConfig . getter)

