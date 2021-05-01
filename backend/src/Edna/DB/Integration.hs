-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

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
  , runDeleteReturningList'
  , runSelectReturningOne'
  , runSelectReturningList'
  ) where

import Universum

import qualified Database.Beam.Postgres.Conduit as C

import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.Beam.Backend.SQL.Row (FromBackendRow)
import Database.Beam.Postgres (Connection, Pg, Postgres, runBeamPostgres, runBeamPostgresDebug)
import Database.Beam.Postgres.Full (PgDeleteReturning, runPgDeleteReturningList)
import Database.Beam.Query
  (SqlDelete, SqlInsert, SqlSelect, SqlUpdate, runDelete, runInsert, runSelectReturningList,
  runSelectReturningOne, runUpdate)
import Database.Beam.Schema (Beamable)

import Edna.DB.Connection (PostgresConn(..), postgresConnSingle)
import Edna.Logging (logUnconditionally)
import Edna.Setup (Edna, EdnaContext(_edDBConnection), edDBConnection, edDebugDB)

withConnection :: (Connection -> Edna a) -> Edna a
withConnection action = do
  c <- view edDBConnection
  pcWithConnection c action

transact :: Edna a -> Edna a
transact action = do
  c <- view edDBConnection
  pcWithConnection c $ \conn ->
    pcWithTransaction c conn $ do
      let c' = postgresConnSingle conn
      local (\ctx -> ctx { _edDBConnection = c' }) $ do
        pcWithTransaction c' conn action

runPg :: Pg a -> Edna a
runPg pg = withConnection $ \conn ->
  view edDebugDB >>= liftIO . \case
    False -> runBeamPostgres conn pg
    True -> runBeamPostgresDebug (logUnconditionally . toText) conn pg

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

runDeleteReturningList' ::
  ( FromBackendRow Postgres a
  ) => PgDeleteReturning a -> Edna [a]
runDeleteReturningList' = runPg . runPgDeleteReturningList

runSelectReturningOne' :: FromBackendRow Postgres a => SqlSelect Postgres a -> Edna (Maybe a)
runSelectReturningOne' = runPg . runSelectReturningOne

runSelectReturningList' :: FromBackendRow Postgres a => SqlSelect Postgres a -> Edna [a]
runSelectReturningList' = runPg . runSelectReturningList
