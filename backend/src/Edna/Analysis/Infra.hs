-- | Infrastructure to call analysis code in Python.

module Edna.Analysis.Infra
  ( PythonError (..)
  , callPythonAnalysis
  ) where

import Universum

import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode, encode)
import Fmt (Buildable(..), pretty)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (StdStream(..), cwd, proc, readCreateProcessWithExitCode, std_err, std_out)

import Edna.Logging (logDebug)
import Edna.Setup (Edna)

-- | General errors that can happen when we call Python code.
-- They indicate a bug in Python code or incorrect environment (e. g. mismatched
-- versions of Haskell and Python code).
data PythonError
  = PyExitFailure ExitCode String
  -- ^ Python process exited with non-zero code.
  | PyInvalidFormat Text
  -- ^ Python process produced output that we can't parse.
  deriving stock (Show, Eq)

instance Buildable PythonError where
  build = \case
    PyExitFailure ec err ->
      "python call exited with " <> build (show @String ec) <>
      ", stderr:\n" <> build err
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
  let requestString = decodeUtf8 $ encode request
  logDebug $ toText $ "Python data request: " <> requestString
  analysisDir <- liftIO $
    fromMaybe (".." </> "analysis") <$> lookupEnv "EDNA_ANALYSIS_DIR"
  (exitCode, out, err) <- liftIO $ readCreateProcessWithExitCode
    (proc "python3" [pyPath, requestString])
    { cwd = Just analysisDir, std_out = CreatePipe, std_err = CreatePipe}
    ""
  let isSuccess = exitCode == ExitSuccess
  -- Logging with debug severity in case of success.
  -- Not logging in case of failure because the whole stderr will be provided
  -- in the thrown exception.
  when (not (null err) && isSuccess) $
    logDebug $ "Unexpected python stderr: " <> toText err
  unless isSuccess $ throwM $ PyExitFailure exitCode err
  logDebug $ toText $ "Python data response: " <> out
  either (throwM . PyInvalidFormat . toText) pure $ eitherDecode (encodeUtf8 out)
