-- | Implementation of 4PL (four parameter logistic) analysis.

module Edna.Analysis.FourPL
  ( Params4PL (..)
  , AnalysisResult
  , Params4PLResp (..)
  , Params4PLReq (..)
  , analyse4PL
  ) where

import Universum

import Data.Aeson (FromJSON(..), ToJSON(..), eitherDecode, encode)
import Data.Aeson.TH (deriveJSON)
import Data.Swagger (ToSchema(..))
import Fmt (Buildable(..), genericF, tupleF)
import Servant.Util.Combinators.Logging (ForResponseLog, buildForResponse)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (StdStream(..), cwd, proc, readCreateProcessWithExitCode, std_err, std_out)

import Edna.Logging (logDebug)
import Edna.Orphans ()
import Edna.Setup (Edna)
import Edna.Util (ExperimentId, ednaAesonWebOptions)

-- | Parameters of 4PL function, analysis outcome of this analysis method.
-- The function is defined as follows:
-- @f(x) = d + (a - d) / (1 + (x / c)^b)@
--
-- 4 fields of this type are @a@, @b@, @c@ and @d@ parameters respectively.
data Params4PL = Params4PL
  { p4plA :: Double
  , p4plB :: Double
  , p4plC :: Double
  , p4plD :: Double
  } deriving stock (Generic, Show, Eq)

type Params4PLTuple = (Double, Double, Double, Double)

toTuple :: Params4PL -> Params4PLTuple
toTuple Params4PL {..} = (p4plA, p4plB, p4plC, p4plD)

instance Buildable Params4PL where
  build = tupleF . toTuple

instance Buildable (ForResponseLog Params4PL) where
  build = buildForResponse

-- Serializing as a tuple for brevity.
instance ToJSON Params4PL where
  toJSON = toJSON . toTuple
  toEncoding = toEncoding . toTuple

instance FromJSON Params4PL where
  parseJSON = fmap constr . parseJSON
    where
      constr (a, b, c, d) = Params4PL a b c d

instance ToSchema Params4PL where
  declareNamedSchema Proxy =
    declareNamedSchema @Params4PLTuple Proxy

type AnalysisResult = Either Text Params4PL

instance Buildable AnalysisResult where
  build = genericF

instance Buildable (ForResponseLog AnalysisResult) where
  build = buildForResponse

data Params4PLReq = Params4PLReq
  { plreqExperiment :: ExperimentId
  , plreqData :: [(Double, Double)]
  } deriving stock (Generic, Show, Eq)

deriveJSON ednaAesonWebOptions ''Params4PLReq

data Params4PLResp = Params4PLResp
  { plrspExperiment :: ExperimentId
  , plrspData :: AnalysisResult
  } deriving stock (Generic, Show, Eq)

deriveJSON ednaAesonWebOptions ''Params4PLResp

callPythonAnalysis :: String -> Edna (Either String [Params4PLResp])
callPythonAnalysis request = do
  logDebug $ toText $ "Python data request: " <> request
  analysisDir <- liftIO $ fromMaybe (".." </> "analysis") <$> (lookupEnv "EDNA_ANALYSIS_DIR")
  (exitCode, out, err) <- liftIO $ readCreateProcessWithExitCode
    (proc "python3" ["analysis" </> "ic50.py", request])
    { cwd = Just analysisDir, std_out = CreatePipe, std_err = CreatePipe}
    ""
  unless (null err) $ logDebug $ "Unexpected python errors: " <> toText err
  pure $ do
    unless (exitCode == ExitSuccess) $
      Left $ "python call exited with " <>
      show exitCode <> ", stderr: " <> err
    eitherDecode $ encodeUtf8 $ toString out

-- | This function performs actual analysis and finds the best parameters.
analyse4PL :: [Params4PLReq] -> Edna [Params4PLResp]
analyse4PL experiments = do
  analysisResults <- callPythonAnalysis $ decodeUtf8 $ encode experiments
  case analysisResults of
    Left parsingError -> error $ toText parsingError
    Right params -> pure params
