-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

{-# LANGUAGE ApplicativeDo #-}

module Main
  ( main
  ) where

import Universum

import Main.Utf8 (withUtf8)
import Options.Applicative
  (Parser, ParserInfo, auto, execParser, footerDoc, fullDesc, help, helper, info, long, option,
  progDesc, showDefault, switch, value)

import Edna.Config.CLA (EdnaOptions, ednaOptsParser)
import Edna.Config.Prepare (prepareConfig)
import Edna.Setup (runEdna)

import Generator (GeneratorSpec(..), generateAndSave, generatorDetails)

main :: IO ()
main = withUtf8 $ do
  (options, generatorSpec) <- execParser parserInfo
  config <- prepareConfig options
  runEdna config $ generateAndSave generatorSpec

----------------
-- Command line
----------------

parserInfo :: ParserInfo (EdnaOptions, GeneratorSpec)
parserInfo = info (parser <**> helper) $
  fullDesc <> progDesc "Sample data generator for Edna" <> footerDoc (Just generatorDetails)
  where
    parser = (,) <$> ednaOptsParser <*> generatorSpecParser

generatorSpecParser :: Parser GeneratorSpec
generatorSpecParser = do
  gsProjectNum <- option auto $ mconcat
    [ long "projects"
    , value 4
    , showDefault
    , help "Total number of projects to generate"
    ]
  gsMethodologyNum <- option auto $ mconcat
    [ long "methodologies"
    , value 6
    , showDefault
    , help "Total number of methodologies to generate"
    ]
  gsCompoundNum <- option auto $ mconcat
    [ long "compounds"
    , value 5
    , showDefault
    , help "Number of compounds in one file"
    ]
  gsTargetNum <- option auto $ mconcat
    [ long "targets"
    , value 5
    , showDefault
    , help "Number of targets in one file"
    ]
  gsMeasurementsNum <- option auto $ mconcat
    [ long "measurements"
    , value 10
    , showDefault
    , help "Number of measurements in one experiment"
    ]
  gsStrict <- switch $ mconcat
    [ long "strict"
    , help "Fail on duplicates instead of skipping them"
    ]
  pure GeneratorSpec {..}
