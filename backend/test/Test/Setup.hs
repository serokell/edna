module Test.Setup
  ( withContext
  , ednaTestMode
  ) where

import Universum

import Control.Monad.Morph (hoist)
import Hedgehog.Internal.Property (PropertyT(..))
import Lens.Micro ((?~))
import RIO (runRIO)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, SpecWith, around)

import Edna.Config.Definition (DbInit(..), dbConnString, dbInitialisation, defaultEdnaConfig, ecDb)
import Edna.DB.Connection (withPostgresConn)
import Edna.DB.Initialisation (schemaInit)
import Edna.Setup (Edna, EdnaContext(..))
import Edna.Util (ConnString(..), DatabaseInitOption(..))

-- | Env variable from which `pg_tmp` temp server connection string
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
      withPostgresConn testConfig $ \connPool -> do
        let ctx = EdnaContext testConfig connPool
        callback ctx

ednaTestMode :: EdnaContext -> PropertyT Edna a -> PropertyT IO ()
ednaTestMode ctx = void . hoist (\action -> runRIO ctx $ schemaInit *> action)
