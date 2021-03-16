module Test.Setup
  ( withContext
  ) where

import Universum

import Lens.Micro ((?~))
import RIO (runRIO)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, SpecWith, around)

import Edna.Config.Definition (DbInit(..), dbConnString, dbInitialisation, defaultEdnaConfig, ecDb)
import Edna.DB.Connection (withPostgresConn)
import Edna.DB.Integration (runPg)
import Edna.DB.Initialisation (resetSchema)
import Edna.Setup (EdnaContext(..))
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

resetDbSchema :: EdnaContext -> IO ()
resetDbSchema context = runRIO context $ runPg resetSchema

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
            ecDb . dbInitialisation ?~ DbInit Enable "./sql/init.sql" &
            ecDb . dbConnString .~ connString
      withPostgresConn testConfig $ \connPool -> do
        let ctx = EdnaContext testConfig connPool
        callback ctx <* resetDbSchema ctx
