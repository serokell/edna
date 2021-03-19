module Edna.DB.Integration
  ( withConnection
  , transact
  , runPg
  , runInsert'
  , runUpdate'
  , runInsertReturningList'
  , runInsertReturningOne'
  , runUpdateAffected'
  , runDelete'
  , runSelectReturningOne'
  , runSelectReturningList'
  ) where

import Universum

import qualified Database.Beam.Postgres.Conduit as C

import Data.Pool (withResource)
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.Beam.Backend.SQL.Row (FromBackendRow)
import Database.Beam.Postgres (Connection, Pg, Postgres, runBeamPostgres)
import Database.Beam.Query
  (SqlDelete, SqlInsert, SqlSelect, SqlUpdate, runDelete, runInsert, runSelectReturningList,
  runSelectReturningOne, runUpdate)
import Database.Beam.Schema (Beamable)
import Database.PostgreSQL.Simple.Transaction (withTransactionSerializable)
import RIO (withRunInIO)

import Edna.DB.Connection (ConnPool(..))
import Edna.Setup (Edna, edConnectionPool)

withConnection :: (Connection -> Edna a) -> Edna a
withConnection action = do
  ConnPool pool <- view edConnectionPool
  withRunInIO $ \unlift -> withResource pool (unlift . action)

transact :: Edna a -> Edna a
transact action = withConnection $
  \conn -> withRunInIO $ \unlift -> withTransactionSerializable conn (unlift action)

runPg :: Pg a -> Edna a
runPg pg = withConnection $ \conn -> liftIO $ runBeamPostgres conn pg

runInsert' :: SqlInsert Postgres table -> Edna ()
runInsert' = runPg . runInsert

runInsertReturningList' ::
  ( Beamable table
  , FromBackendRow Postgres (table Identity)
  ) => SqlInsert Postgres table -> Edna [table Identity]
runInsertReturningList' = runPg . runInsertReturningList

-- | Insert items and expect one item in the result. The caller is responsible
-- for ensuring that exactly one result will be returned.
runInsertReturningOne' ::
  ( Beamable table
  , FromBackendRow Postgres (table Identity)
  ) => SqlInsert Postgres table -> Edna (table Identity)
runInsertReturningOne' = fmap expectOneInsertion . runInsertReturningList'
  where
    expectOneInsertion :: HasCallStack => [x] -> x
    expectOneInsertion = \case
      [x] -> x
      xs -> error $
        "Expected to insert 1 item, but inserted: " <> show (length xs)

runUpdate' :: SqlUpdate Postgres tbl -> Edna ()
runUpdate' = runPg . runUpdate

runUpdateAffected' :: SqlUpdate Postgres tbl -> Edna Int64
runUpdateAffected' pg = withConnection $ \conn -> liftIO $ C.runUpdate conn pg

runDelete' :: SqlDelete Postgres tbl -> Edna ()
runDelete' = runPg . runDelete

runSelectReturningOne' :: FromBackendRow Postgres a => SqlSelect Postgres a -> Edna (Maybe a)
runSelectReturningOne' = runPg . runSelectReturningOne

runSelectReturningList' :: FromBackendRow Postgres a => SqlSelect Postgres a -> Edna [a]
runSelectReturningList' = runPg . runSelectReturningList
