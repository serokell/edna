module Test.Setup
  ( withContext
  , runWithInit
  , ednaTestMode
  , runTestEdna
  ) where

import Universum

import Control.Monad.Morph (hoist)
import Hedgehog.Internal.Property (PropertyT(..))
import Lens.Micro ((?~))
import RIO (runRIO)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, SpecWith, around)

import Edna.Config.Definition (DbInit(..), dbConnString, dbInitialisation, defaultEdnaConfig, ecDb)
import Edna.DB.Initialisation (schemaInit)
import Edna.Setup (Edna, EdnaContext(..), runEdna)
import Edna.Util (ConnString(..), DatabaseInitOption(..))

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

-- | Provide 'EdnaContext' to a spec. It's based on the default config,
-- but uses a custom connection string specifically for tests.
-- It initializes DB and resets it in the end.
withContext :: SpecWith EdnaContext -> Spec
withContext = around withContext'
  where
    withContext' :: (EdnaContext -> IO a) -> IO a
    withContext' callback = do
      connString <- postgresTestServerConnString
      let testConfig = defaultEdnaConfig &
            ecDb . dbInitialisation ?~ DbInit EnableWithDrop "./sql/init.sql" &
            ecDb . dbConnString .~ connString
      runEdna testConfig $ do
        ctx <- ask
        liftIO $ callback ctx

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
