-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Edna.Library.DB.Query
  ( getTargetById
  , getTargets
  , getCompoundById
  , getCompounds
  , editCompoundChemSoft
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

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Database.Beam.Postgres.Full as Pg

import Database.Beam.Backend (SqlSerial(..))
import Database.Beam.Postgres (now_, pgNubBy_)
import Database.Beam.Query
  (all_, asc_, cast_, default_, filter_, guard_, insert, insertExpressions, int, just_, leftJoin_,
  lookup_, orderBy_, select, update, val_, (<-.), (==.))

import Edna.DB.Integration
  (runDeleteReturningList', runInsert', runInsertReturningOne', runSelectReturningList',
  runSelectReturningOne', runUpdate')
import Edna.DB.Schema (EdnaSchema(..), ednaSchema)
import Edna.Dashboard.DB.Schema (ExperimentT(..))
import Edna.Library.DB.Schema as LDB
  (CompoundRec, CompoundT(..), PrimaryKey(..), ProjectRec, ProjectT(..), TargetRec, TargetT(..),
  TestMethodologyRec, TestMethodologyT(..))
import Edna.Library.Web.Types (MethodologyReq(..), ProjectReq(..), ProjectResp(..), TargetResp(..))
import Edna.Setup (Edna)
import Edna.Upload.DB.Schema (ExperimentFileT(..))
import Edna.Util as U
  (CompoundId, IdType(..), MethodologyId, ProjectId, SqlId(..), TargetId, justOrError, localToUTC)
import Edna.Util.URI (renderURI)
import Edna.Web.Types (WithId(..))

--------------------------
-- Target
--------------------------

targetToDomain :: TargetId -> TargetRec -> [Maybe ProjectRec] -> WithId 'U.TargetId TargetResp
targetToDomain targetSqlId TargetRec{..} projects = WithId targetSqlId $ TargetResp
  { trName = tName
  , trProjects = mapMaybe (fmap pName) projects
  , trAdditionDate = localToUTC tAdditionDate
  }

-- TODO maybe we should move it to Service layer
-- | Return API value of the target by its ID
getTargetById :: TargetId -> Edna (Maybe (WithId 'U.TargetId TargetResp))
getTargetById targetSqlId = do
  targetWithProjects <- targetsWithProjects $ Just targetSqlId
  case targetWithProjects of
    [] -> pure Nothing
    xs@((target, _) : _) -> pure $ Just $ targetToDomain targetSqlId target $ map snd xs

-- TODO maybe we should move it to Service layer
-- | Return API values of all targets
getTargets :: Edna [WithId 'U.TargetId TargetResp]
getTargets = do
  targets <- targetsWithProjects Nothing
  let groupedTargets =
        L.groupBy (\(t1, _) (t2, _) -> tTargetId t1 == tTargetId t2) targets
  pure $ foldr getTarget [] groupedTargets
  where
    getTarget target targets = case target of
      xs@((t, _) : _) ->
        targetToDomain (SqlId $ unSerial $ tTargetId t) t (map snd xs) : targets
      _ -> targets

-- | Combines targets with projects to which they relate
-- If specific target was passed, use only this target in query
targetsWithProjects :: Maybe TargetId -> Edna [(TargetRec, Maybe ProjectRec)]
targetsWithProjects targetSqlId = runSelectReturningList' $ select $
  orderBy_ (\(t, _) -> asc_ $ tTargetId t) $
  pgNubBy_ (bimap tTargetId pProjectId) $
  filter_ specificTarget do
    let EdnaSchema {..} = ednaSchema
    targets <- all_ esTarget
    experiments <- leftJoin_ (all_ esExperiment) $
      \e -> eTargetId e ==. cast_ (tTargetId targets) int
    files <- leftJoin_ (all_ esExperimentFile) $
      \f -> just_ (cast_ (efExperimentFileId f) int) ==. eExperimentFileId experiments
    projects <- leftJoin_ (all_ esProject) $
      \p -> just_ (cast_ (pProjectId p) int) ==. efProjectId files
    pure (targets, projects)
  where
    specificTarget = case targetSqlId of
      Just (SqlId targetId) -> \(t, _) ->
        tTargetId t ==. val_ (SqlSerial targetId)
      Nothing -> \_ -> val_ True

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

-- | Get all compounds
getCompounds :: Edna [CompoundRec]
getCompounds = runSelectReturningList' $ select $ all_ $ esCompound ednaSchema

-- | Edit ChemSoft link of a given compound
editCompoundChemSoft :: CompoundId -> Text -> Edna ()
editCompoundChemSoft (SqlId compoundId) link = runUpdate' $ update (esCompound ednaSchema)
  (\c -> cChemsoftLink c <-. val_ (Just link))
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
    (insertExpressions [CompoundRec default_ (val_ compoundName) default_ default_])
    (Pg.onConflict (Pg.conflictingFields cName) Pg.onConflictDoNothing)
  getCompoundByName compoundName >>= justOrError ("added compound not found: " <> compoundName)

--------------------------
-- Test methodology
--------------------------

-- | Get methodology by its name. Return nothing if there is no such methodology.
getMethodologyById :: MethodologyId -> Edna (Maybe (TestMethodologyRec, [Text]))
getMethodologyById = fmap listToMaybe . getMethodology' . Just

-- | Get methodology by its name. Return nothing if there is no such methodology.
getMethodologyByName :: Text -> Edna (Maybe TestMethodologyRec)
getMethodologyByName name = runSelectReturningOne' $ select $ do
  methodologies <- all_ $ esTestMethodology ednaSchema
  guard_ (LDB.tmName methodologies ==. val_ name)
  pure methodologies

-- | Get all methodologies
getMethodologies :: Edna [(TestMethodologyRec, [Text])]
getMethodologies = getMethodology' Nothing

getMethodology' :: Maybe MethodologyId -> Edna [(TestMethodologyRec, [Text])]
getMethodology' mMethodologyId =
  fmap convert $
  runSelectReturningList' $ select $
  pgNubBy_ (bimap tmTestMethodologyId pProjectId) $ do
    tm <- all_ $ esTestMethodology ednaSchema
    whenJust mMethodologyId $ \(SqlId methodId) ->
      guard_ (tmTestMethodologyId tm ==. val_ (SqlSerial methodId))
    experimentFile <- leftJoin_ (all_ $ esExperimentFile ednaSchema) $
      \ef -> just_ (cast_ (tmTestMethodologyId tm) int) ==. efMethodologyId ef
    project <- leftJoin_ (all_ $ esProject ednaSchema) $
      \p -> just_ (cast_ (pProjectId p) int) ==. efProjectId experimentFile
    return (tm, project)
  where
    convert :: [(TestMethodologyRec, Maybe ProjectRec)] -> [(TestMethodologyRec, [Text])]
    convert =
      map (\ne -> (fst $ head ne, map pName . mapMaybe snd . toList $ ne)) .
      NE.groupAllWith (tmTestMethodologyId . fst)

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

projectToDomain
  :: ProjectId
  -> ProjectRec
  -> [Maybe CompoundRec]
  -> WithId 'U.ProjectId ProjectResp
projectToDomain projectSqlId ProjectRec{..} compounds = WithId projectSqlId $ ProjectResp
  { prName = pName
  , prDescription = pDescription
  , prCreationDate = localToUTC pCreationDate
  , prLastUpdate = localToUTC pLastUpdate
  , prCompoundNames = mapMaybe (fmap cName) compounds
  }

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

-- TODO maybe we should move it to Service layer
-- | Return API value of the project by its ID
getProjectWithCompoundsById :: ProjectId -> Edna (Maybe (WithId 'U.ProjectId ProjectResp))
getProjectWithCompoundsById projectSqlId = do
  projectWithCompounds <- projectsWithCompounds $ Just projectSqlId
  case projectWithCompounds of
    [] -> pure Nothing
    xs@((project, _) : _) -> pure $ Just $ projectToDomain projectSqlId project $ map snd xs

-- TODO maybe we should move it to Service layer
-- | Return API values of all projects
getProjectsWithCompounds :: Edna [WithId 'U.ProjectId ProjectResp]
getProjectsWithCompounds = do
  projects <- projectsWithCompounds Nothing
  let groupedProjects = L.groupBy
        (\(p1, _) (p2, _) -> pProjectId p1 == pProjectId p2) projects
  pure $ foldr getProject [] groupedProjects
  where
    getProject project projects = case project of
      xs@((p, _) : _) ->
        projectToDomain (SqlId $ unSerial $ pProjectId p) p (map snd xs) : projects
      _ -> projects

-- | Combines project with compounds to which they relate
-- If specific project was passed, use only this project in query
projectsWithCompounds :: Maybe ProjectId -> Edna [(ProjectRec, Maybe CompoundRec)]
projectsWithCompounds projectSqlId = runSelectReturningList' $ select $
  orderBy_ (\(t, _) -> asc_ $ pProjectId t) $
  pgNubBy_ (bimap pProjectId cCompoundId) $
  filter_ specificProject do
    let EdnaSchema {..} = ednaSchema
    projects <- all_ esProject
    files <- leftJoin_ (all_ esExperimentFile) $
      \f -> efProjectId f ==. cast_ (pProjectId projects) int
    experiments <- leftJoin_ (all_ esExperiment) $
      \e -> eExperimentFileId e ==. cast_ (efExperimentFileId files) int
    compounds <- leftJoin_ (all_ esCompound) $
      \c -> just_ (cast_ (cCompoundId c) int) ==. eCompoundId experiments
    pure (projects, compounds)
  where
    specificProject = case projectSqlId of
      Just (SqlId projectId) -> \(t, _) -> pProjectId t ==. val_ (SqlSerial projectId)
      Nothing -> \_ -> val_ True

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
