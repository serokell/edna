-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- beam types are just too complex
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Edna.Library.DB.Query
  ( getTargetById
  , getTargets
  , getCompoundById
  , getCompounds
  , editCompoundChemSoft
  , editCompoundMde
  , getMethodologyById
  , getMethodologyByName
  , getMethodologies
  , deleteMethodology
  , insertMethodology
  , updateMethodology
  , getProjectById
  , getProjectByName
  , getProjectWithCompoundsById
  , getProjectsWithCompounds
  , insertProject
  , updateProject
  , touchProject
  , insertTarget
  , getTargetByName
  , getCompoundByName
  , insertCompound
  ) where

import Universum

import qualified Database.Beam.Postgres.Full as Pg

import Database.Beam.Backend (SqlSerial(..))
import Database.Beam.Postgres (Postgres, now_)
import Database.Beam.Query
  (Q, all_, cast_, default_, filter_, guard_, insert, insertExpressions, int, just_, leftJoin_,
  lookup_, select, update, val_, (<-.), (==.))
import Servant.Util (HList(..), PaginationSpec(..), (.*.))
import Servant.Util.Beam.Postgres (paginate_, sortBy_)
import Servant.Util.Combinators.Sorting.Backend (fieldSort)

import Edna.DB.Integration
  (runDeleteReturningList', runInsert', runInsertReturningOne', runSelectReturningList',
  runSelectReturningOne', runUpdate')
import Edna.DB.Schema (EdnaSchema(..), ednaSchema)
import Edna.DB.Util (groupAndPaginate)
import Edna.Dashboard.DB.Schema (ExperimentT(..))
import Edna.Library.DB.Schema as LDB
  (CompoundRec, CompoundT(..), PrimaryKey(..), ProjectRec, ProjectT(..), TargetRec, TargetT(..),
  TestMethodologyRec, TestMethodologyT(..))
import Edna.Library.Web.Types
  (CompoundSortingSpec, MethodologyReq(..), MethodologySortingSpec, ProjectReq(..),
  ProjectSortingSpec, TargetSortingSpec)
import Edna.Setup (Edna)
import Edna.Upload.DB.Schema (ExperimentFileT(..))
import Edna.Util as U (CompoundId, MethodologyId, ProjectId, SqlId(..), TargetId, justOrError)
import Edna.Util.URI (renderURI)

-- General notes for this module:
--
-- 1. We can't use @pgNubBy_@ in queries returning @(MainItem, [ItemsBelongToIt])@ because
-- "SELECT DISTINCT ON expressions must match initial ORDER BY expressions".
-- 2. Also in such cases we can't use @paginate_@ easily, because we want to paginate
-- only main items, but we may get @[(mainItem, smth1), (mainItem, smth2)]@ as
-- two different items which we want to treat as one item.

--------------------------
-- Target
--------------------------

-- | Return target by its ID along with all projects where this target is involved.
getTargetById :: TargetId -> Edna (Maybe (TargetRec, [Text]))
getTargetById = fmap listToMaybe . targetsWithProjects . Left

-- | Return all targets sorted and paginated according to
-- provided specifications along with projects where targets are involved.
getTargets ::
  TargetSortingSpec -> PaginationSpec -> Edna [(TargetRec, [Text])]
getTargets sorting pagination =
  targetsWithProjects $ Right (sorting, pagination)

-- | Combines targets with projects to which they relate.
-- If specific target was passed, use only this target in query.
-- Otherwise, pagination and sorting parameters are passed and taken into account.
targetsWithProjects ::
  Either TargetId (TargetSortingSpec, PaginationSpec) -> Edna [(TargetRec, [Text])]
targetsWithProjects targetIdEither =
  groupAndPaginate (snd <$> rightToMaybe targetIdEither) (unSerial . tTargetId) <$>
  case targetIdEither of
    Left targetSqlId ->
      runSelectReturningList' $ select $
      filter_ (specificTarget targetSqlId) baseQuery
    Right (sorting, _) ->
      runSelectReturningList' $ select $
      sortBy_ sorting sortingApp baseQuery
  where
    baseQuery :: Q Postgres EdnaSchema s _
    baseQuery = do
      let EdnaSchema {..} = ednaSchema
      targets <- all_ esTarget
      experiments <- leftJoin_ (all_ esExperiment) $
        \e -> eTargetId e ==. cast_ (tTargetId targets) int
      files <- leftJoin_ (all_ esExperimentFile) $
        \f -> just_ (cast_ (efExperimentFileId f) int) ==. eExperimentFileId experiments
      projects <- leftJoin_ (all_ esProject) $
        \p -> just_ (cast_ (pProjectId p) int) ==. efProjectId files
      pure (targets, pName projects)

    specificTarget (SqlId targetId) (t, _)=
      tTargetId t ==. val_ (SqlSerial targetId)

    sortingApp (TargetRec {..}, _) =
      fieldSort @"name" tName .*.
      fieldSort @"additionDate" tAdditionDate .*.
      HNil

-- | Get target by its name. Return nothing if there is no such target.
getTargetByName :: Text -> Edna (Maybe TargetRec)
getTargetByName name = runSelectReturningOne' $ select $ do
  targets <- all_ $ esTarget ednaSchema
  guard_ (LDB.tName targets ==. val_ name)
  pure targets

-- | Insert target with given name and return its DB value. If target with this name
-- already exists do nothing and simply return it.
insertTarget :: Text -> Edna TargetRec
insertTarget targetName = do
  runInsert' $ Pg.insert
    (esTarget ednaSchema)
    (insertExpressions [TargetRec default_ (val_ targetName) default_])
    (Pg.onConflict (Pg.conflictingFields tName) Pg.onConflictDoNothing)
  getTargetByName targetName >>= justOrError ("added target not found: " <> targetName)

--------------------------
-- Compound
--------------------------

-- | Get compound by its ID. Return nothing if there is no such compound.
getCompoundById :: CompoundId -> Edna (Maybe CompoundRec)
getCompoundById (SqlId compoundId) = runSelectReturningOne' $ select $ do
  compounds <- all_ $ esCompound ednaSchema
  guard_ (cCompoundId compounds ==. val_ (SqlSerial compoundId))
  pure compounds

-- | Get compounds sorted and paginated according to provided specifications.
getCompounds :: CompoundSortingSpec -> PaginationSpec -> Edna [CompoundRec]
getCompounds sorting pagination = runSelectReturningList' $ select $
  paginate_ pagination $ sortBy_ sorting sortingApp $ all_ $ esCompound ednaSchema
  where
    sortingApp CompoundRec {..} =
      fieldSort @"name" cName .*.
      fieldSort @"additionDate" cAdditionDate .*.
      HNil

-- | Edit ChemSoft link of a given compound
editCompoundChemSoft :: CompoundId -> Text -> Edna ()
editCompoundChemSoft (SqlId compoundId) link = runUpdate' $ update (esCompound ednaSchema)
  (\c -> cChemsoftLink c <-. val_ (Just link))
  (\c -> cCompoundId c ==. val_ (SqlSerial compoundId))

-- | Edit Mde link of a given compound
editCompoundMde :: CompoundId -> Text -> Edna ()
editCompoundMde (SqlId compoundId) link = runUpdate' $ update (esCompound ednaSchema)
  (\c -> cMdeLink c <-. val_ (Just link))
  (\c -> cCompoundId c ==. val_ (SqlSerial compoundId))

-- | Get compound by its name. Return nothing if there is no such compound.
getCompoundByName :: Text -> Edna (Maybe CompoundRec)
getCompoundByName name = runSelectReturningOne' $ select $ do
  compounds <- all_ $ esCompound ednaSchema
  guard_ (LDB.cName compounds ==. val_ name)
  pure compounds

-- | Insert compound with given name and return its DB value. If compound with this name
-- already exists do nothing and simply return it.
insertCompound :: Text -> Edna CompoundRec
insertCompound compoundName = do
  runInsert' $ Pg.insert
    (esCompound ednaSchema)
    (insertExpressions [CompoundRec default_ (val_ compoundName) default_ default_ default_])
    (Pg.onConflict (Pg.conflictingFields cName) Pg.onConflictDoNothing)
  getCompoundByName compoundName >>= justOrError ("added compound not found: " <> compoundName)

--------------------------
-- Test methodology
--------------------------

-- | Get methodology by its name. Return nothing if there is no such methodology.
getMethodologyById :: MethodologyId -> Edna (Maybe (TestMethodologyRec, [Text]))
getMethodologyById = fmap listToMaybe . getMethodology' . Left

-- | Get methodology by its name. Return nothing if there is no such methodology.
getMethodologyByName :: Text -> Edna (Maybe TestMethodologyRec)
getMethodologyByName name = runSelectReturningOne' $ select $ do
  methodologies <- all_ $ esTestMethodology ednaSchema
  guard_ (LDB.tmName methodologies ==. val_ name)
  pure methodologies

-- | Get methodologies sorted and paginated according to provided specifications.
getMethodologies ::
  MethodologySortingSpec -> PaginationSpec -> Edna [(TestMethodologyRec, [Text])]
getMethodologies sorting pagination =
  getMethodology' $ Right (sorting, pagination)

getMethodology' ::
  Either MethodologyId (MethodologySortingSpec, PaginationSpec) ->
  Edna [(TestMethodologyRec, [Text])]
getMethodology' eMethodologyId =
  groupAndPaginate (snd <$> rightToMaybe eMethodologyId) (unSerial . tmTestMethodologyId) <$>
  case eMethodologyId of
    Left _ -> runSelectReturningList' $ select baseQuery
    Right (sorting, _) ->
      runSelectReturningList' $ select $
      sortBy_ sorting sortingApp baseQuery
  where
    baseQuery :: Q Postgres EdnaSchema s _
    baseQuery = do
      tm <- all_ $ esTestMethodology ednaSchema
      whenLeft eMethodologyId $ \(SqlId methodId) ->
        guard_ (tmTestMethodologyId tm ==. val_ (SqlSerial methodId))
      experimentFile <- leftJoin_ (all_ $ esExperimentFile ednaSchema) $
        \ef -> just_ (cast_ (tmTestMethodologyId tm) int) ==. efMethodologyId ef
      project <- leftJoin_ (all_ $ esProject ednaSchema) $
        \p -> just_ (cast_ (pProjectId p) int) ==. efProjectId experimentFile
      return (tm, pName project)

    sortingApp (TestMethodologyRec {..}, _) =
      fieldSort @"name" tmName .*.
      HNil

-- | Insert methodology and return its DB value.
-- Fails if methodology with this name already exists
insertMethodology :: MethodologyReq -> Edna TestMethodologyRec
insertMethodology MethodologyReq{..} = runInsertReturningOne' $
  insert (esTestMethodology ednaSchema) $ insertExpressions
    [ TestMethodologyRec
      { tmTestMethodologyId = default_
      , tmName = val_ mrqName
      , tmDescription = val_ mrqDescription
      , tmConfluenceLink = val_ $ renderURI <$> mrqConfluence
      }
    ]

-- | Update methodology by its ID
updateMethodology :: MethodologyId -> MethodologyReq -> Edna ()
updateMethodology (SqlId methodologyId) MethodologyReq{..} =
  runUpdate' $ update (esTestMethodology ednaSchema)
    (\tm -> mconcat
      [ LDB.tmName tm <-. val_ mrqName
      , LDB.tmDescription tm <-. val_ mrqDescription
      , LDB.tmConfluenceLink tm <-. val_ (renderURI <$> mrqConfluence)])
    (\tm -> tmTestMethodologyId tm ==. val_ (SqlSerial methodologyId))

-- | Delete methodology by its ID. Returns whether something was actually
-- deleted (i. e. methodology with given ID existed before this function call).
deleteMethodology :: MethodologyId -> Edna Bool
deleteMethodology (SqlId methodologyId) =
  fmap (not . null) $ runDeleteReturningList' $
  Pg.deleteReturning (esTestMethodology ednaSchema)
  (\m -> tmTestMethodologyId m ==. val_ (SqlSerial methodologyId))
  tmTestMethodologyId

--------------------------
-- Project
--------------------------

-- | Get project by its ID. Return nothing if there is no such project.
getProjectById :: ProjectId -> Edna (Maybe ProjectRec)
getProjectById (SqlId projectId) = runSelectReturningOne' $
  lookup_ (esProject ednaSchema) $ LDB.ProjectId $ SqlSerial projectId

-- | Get project by its name. Return nothing if there is no such project.
getProjectByName :: Text -> Edna (Maybe ProjectRec)
getProjectByName name = runSelectReturningOne' $ select $ do
  projects <- all_ $ esProject ednaSchema
  guard_ (pName projects ==. val_ name)
  pure projects

-- | Return project by its ID along with all compounds used in this project.
getProjectWithCompoundsById :: ProjectId -> Edna (Maybe (ProjectRec, [Text]))
getProjectWithCompoundsById = fmap listToMaybe . projectsWithCompounds . Left

-- | Return all projects sorted and paginated according to
-- provided specifications along with compounds used in these projects.
getProjectsWithCompounds ::
  ProjectSortingSpec -> PaginationSpec -> Edna [(ProjectRec, [Text])]
getProjectsWithCompounds sorting pagination =
  projectsWithCompounds $ Right (sorting, pagination)

-- | Combines project with compounds to which they relate.
-- If specific project was passed, use only this project in query.
-- Otherwise, pagination and sorting parameters are passed and taken into account.
projectsWithCompounds ::
  Either ProjectId (ProjectSortingSpec, PaginationSpec) ->
  Edna [(ProjectRec, [Text])]
projectsWithCompounds projectIdEither =
  groupAndPaginate (snd <$> rightToMaybe projectIdEither) (unSerial . pProjectId) <$>
  case projectIdEither of
    Left projectSqlId ->
      runSelectReturningList' $ select $
      filter_ (specificProject projectSqlId) baseQuery
    Right (sorting, _) ->
      runSelectReturningList' $ select $ sortBy_ sorting sortingApp baseQuery
  where
    baseQuery :: Q Postgres EdnaSchema s _
    baseQuery = do
      let EdnaSchema {..} = ednaSchema
      projects <- all_ esProject
      files <- leftJoin_ (all_ esExperimentFile) $
        \f -> efProjectId f ==. cast_ (pProjectId projects) int
      experiments <- leftJoin_ (all_ esExperiment) $
        \e -> eExperimentFileId e ==. cast_ (efExperimentFileId files) int
      compounds <- leftJoin_ (all_ esCompound) $
        \c -> just_ (cast_ (cCompoundId c) int) ==. eCompoundId experiments
      pure (projects, cName compounds)

    specificProject (SqlId projectId) (p, _)=
      pProjectId p ==. val_ (SqlSerial projectId)

    sortingApp (ProjectRec {..}, _) =
      fieldSort @"name" pName .*.
      fieldSort @"creationDate" pCreationDate .*.
      fieldSort @"lastUpdate" pLastUpdate .*.
      HNil

-- | Insert project and return its DB value
-- Fails if project with this name already exists
insertProject :: ProjectReq -> Edna ProjectRec
insertProject ProjectReq{..} = runInsertReturningOne' $
  insert (esProject ednaSchema) $ insertExpressions
    [ ProjectRec
      { pProjectId = default_
      , pName = val_ prqName
      , pDescription = val_ prqDescription
      , pCreationDate = default_
      , pLastUpdate = default_
      }
    ]

-- | Update project by its ID
updateProject :: ProjectId -> ProjectReq -> Edna ()
updateProject (SqlId projectId) ProjectReq{..} =
  runUpdate' $ update (esProject ednaSchema)
    (\p -> mconcat
      [ LDB.pName p <-. val_ prqName
      , LDB.pDescription p <-. val_ prqDescription
      , LDB.pLastUpdate p <-. now_])
    (\p -> pProjectId p ==. val_ (SqlSerial projectId))

-- | Update timestamp of last modification of the project
touchProject :: ProjectId -> Edna ()
touchProject (SqlId projectId) =
  runUpdate' $ update (esProject ednaSchema)
    (\p -> LDB.pLastUpdate p <-. now_)
    (\p -> pProjectId p ==. val_ (SqlSerial projectId))
