-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Test.Setup
  ( ednaTestMode
  , runInit
  , runTestEdna
  , runWithInit
  , specWithContextAndEnv
  , specWithContext
  ) where

import Universum

import Control.Lens ((?~))
import Control.Monad.Morph (hoist)
import Hedgehog.Internal.Property (PropertyT(..))
import RIO (runRIO)
import Servant.Client (ClientEnv)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, SpecWith)
import Test.Hspec.Core.Hooks (aroundAll)

import Edna.Config.Definition
  (DbInit(..), EdnaConfig, LoggingConfig(LogNothing), dbConnString, dbInitialisation,
  defaultEdnaConfig, ecDb, ecLogging)
import Edna.DB.Initialisation (schemaInit)
import Edna.Setup (Edna, EdnaContext(..), runEdna)
import Edna.Util (ConnString(..), DatabaseInitOption(..))
import Network.Wai.Handler.Warp (testWithApplication)
import Test.API.Util (app, clientEnv)

-- | Env variable from which @pg_tmp@ temp server connection string
-- is read.
postgresTestServerEnvName :: String
postgresTestServerEnvName = "TEST_PG_CONN_STRING"

-- | Action which reads tmp server connection string from the env var.
postgresTestServerConnString :: IO ConnString
postgresTestServerConnString = lookupEnv postgresTestServerEnvName >>= \case
  Nothing -> error $ "Connection string for test server is not provided. \
                     \Pass it via " <> show postgresTestServerEnvName <>
                     " environmental variable."
  Just res -> do
    when (null res) $
      putTextLn "Warning: empty connection string to postgres server specified"
    pure $ ConnString $ encodeUtf8 res

-- | Provide 'EdnaConfig' which is based on the default config, but uses a
-- custom connection string specifically for tests.
testConfig :: IO EdnaConfig
testConfig = do
  connString <- postgresTestServerConnString
  return $ defaultEdnaConfig &
    ecDb . dbInitialisation ?~ DbInit EnableWithDrop "./sql/init.sql" &
    ecDb . dbConnString .~ connString &
    ecLogging .~ LogNothing

-- | Provide 'EdnaContext' to a spec.
-- It initializes DB and resets it in the end.
specWithContext :: SpecWith EdnaContext -> Spec
specWithContext = aroundAll withContext
  where
    withContext :: (EdnaContext -> IO a) -> IO a
    withContext callback = do
      config <- testConfig
      runEdna config $ do
        ctx <- ask
        liftIO $ callback ctx

-- | Provide 'EdnaContext' and 'ClientEnv' to a spec.
-- It initializes DB and resets it in the end.
specWithContextAndEnv :: SpecWith (EdnaContext, ClientEnv) -> Spec
specWithContextAndEnv = aroundAll withContext
  where
    withContext :: ((EdnaContext, ClientEnv) -> IO a) -> IO a
    withContext callback = do
      config <- testConfig
      runEdna config $ do
        ctx <- ask
        liftIO $ testWithApplication (app ctx) $ \port -> do
          env <- clientEnv port
          callback (ctx, env)

-- | Drop existing DB and initialize it
runInit :: EdnaContext -> IO ()
runInit ctx = runRIO ctx schemaInit

-- | Drop existing DB, initialize it and then run given 'Edna' action.
runWithInit :: EdnaContext -> Edna a -> IO a
runWithInit ctx action = runRIO ctx $ schemaInit *> action

ednaTestMode :: EdnaContext -> PropertyT Edna a -> PropertyT IO ()
ednaTestMode ctx = void . hoist (runWithInit ctx)

-- | This helper is convenient to use to construct argument to 'Test.Hspec.it'
-- inside @SpecWith EdnaContext@. You can write test's body inside the 'Edna'
-- monad.
runTestEdna :: Edna () -> EdnaContext -> IO ()
runTestEdna = flip runRIO
