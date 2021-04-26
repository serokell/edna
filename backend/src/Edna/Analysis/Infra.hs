-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Infrastructure to call analysis code in Python.

module Edna.Analysis.Infra
  ( PythonError (..)
  , callPythonAnalysis
  ) where

import Universum

import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode, encode)
import Fmt (Buildable(..), pretty)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Process.Typed (byteStringInput, proc, readProcess_, setStdin, setWorkingDir)

import Edna.Logging (logDebug)
import Edna.Setup (Edna)

-- | General errors that can happen when we call Python code.
-- They indicate a bug in Python code or incorrect environment (e. g. mismatched
-- versions of Haskell and Python code).
data PythonError
  = PyInvalidFormat Text
  -- ^ Python process produced output that we can't parse.
  deriving stock (Show, Eq)

instance Buildable PythonError where
  build = \case
    PyInvalidFormat err -> "failed to decode python output: " <> build err

instance Exception PythonError where
  displayException = pretty

-- | Call any Python script assuming the following interface:
--
-- 1. Input to the script is JSON-encoded and passed as the first command line
-- argument.
-- 2. Output is read from stdout and parsed from JSON.
--
-- If output decoding fails or Python exits with non-zero code, an error is
-- returned as a @Left@ value.
-- Path passed to this function must be relative to the entry point for Python
-- code (where Poetry config is located).
callPythonAnalysis ::
  (ToJSON inp, FromJSON out) => FilePath -> inp -> Edna out
callPythonAnalysis pyPath request = do
  let requestJSON = encode request
  logDebug $ "Python data request: " <> decodeUtf8 requestJSON
  analysisDir <- liftIO $
    fromMaybe (".." </> "analysis") <$> lookupEnv "EDNA_ANALYSIS_DIR"
  let processConfig =
        setStdin (byteStringInput requestJSON) $
        setWorkingDir analysisDir $
        proc "python3" [pyPath]
  -- @readProcess_@ automatically captures stdout and stderr.
  -- It also checks the exit code and throws an exception if it's not 0.
  (out, err) <- readProcess_ processConfig
  unless (null err) $
    logDebug $ "Unexpected python stderr: " <> decodeUtf8 err
  logDebug $ "Python data response: " <> decodeUtf8 out
  either (throwM . PyInvalidFormat . toText) pure $ eitherDecode out
