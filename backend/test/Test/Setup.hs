module Test.Setup
  ( withContext
  ) where

import Universum

import RIO (runRIO)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, SpecWith, afterAll, beforeAll)

import Edna.Config.Definition (defaultEdnaConfig)
import Edna.DB.Connection (ConnPool(..), createConnPool, destroyConnPool)
import Edna.DB.Integration (runPg)
import Edna.DB.Schema (ensureSchemaIsSetUp, resetSchema)
import Edna.Setup (EdnaContext(..), edConnectionPool)
import Edna.Util (ConnString(..))

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

postgresTestDbConnections :: Int
postgresTestDbConnections = 200

setupDbConnection :: IO ConnPool
setupDbConnection = do
  connString <- postgresTestServerConnString
  createConnPool connString postgresTestDbConnections

setupDbSchema :: EdnaContext -> IO EdnaContext
setupDbSchema context = do
  runRIO context $ runPg ensureSchemaIsSetUp
  pure context

resetDbSchema :: EdnaContext -> IO ()
resetDbSchema context = runRIO context $ runPg resetSchema

resetConnection :: EdnaContext -> IO ()
resetConnection = destroyConnPool . (^. edConnectionPool)

withContext :: SpecWith EdnaContext -> Spec
withContext =
  beforeAll (setupDbConnection >>= (pure . EdnaContext defaultEdnaConfig) >>= setupDbSchema) .
  afterAll (resetDbSchema <> resetConnection)

