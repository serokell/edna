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
  , insertTarget
  , getTargetByName
  , getCompoundByName
  , insertCompound
  ) where

import Universum

import qualified Data.List as L
import qualified Database.Beam.Postgres.Full as Pg

import Database.Beam.Backend (SqlSerial(..))
import Database.Beam.Postgres (pgNubBy_)
import Database.Beam.Query
  (all_, asc_, cast_, default_, delete, filter_, guard_, insert, insertExpressions, int, just_,
  leftJoin_, lookup_, orderBy_, select, update, val_, (<-.), (==.))

import Edna.DB.Integration
  (runDelete', runInsert', runInsertReturningOne', runSelectReturningList', runSelectReturningOne',
  runUpdate')
import Edna.DB.Schema (EdnaSchema(..), ExperimentT(..), ednaSchema)
import Edna.Library.DB.Schema as LDB
  (CompoundRec, CompoundT(..), PrimaryKey(..), ProjectRec, ProjectT(..), TargetRec, TargetT(..),
  TestMethodologyRec, TestMethodologyT(..))
import Edna.Library.Web.Types
  (MethodologyReqResp(..), ProjectReq(..), ProjectResp(..), TargetResp(..))
import Edna.Setup (Edna)
import Edna.Upload.DB.Schema (ExperimentFileT(..))
import Edna.Util as U (CompoundId, IdType(..), ProjectId, SqlId(..), TargetId, justOrError)
import Edna.Util.URI (renderURI)
import Edna.Web.Types (WithId(..))

--------------------------
-- Target
--------------------------

targetToDomain :: TargetId -> TargetRec -> [Maybe ProjectRec] -> WithId 'U.TargetId TargetResp
targetToDomain targetSqlId TargetRec{..} projects = WithId targetSqlId $ TargetResp
  { trName = tName
  , trProjects = mapMaybe (fmap pName) projects
  , trAdditionDate = tAdditionDate
  }

getTargetById :: TargetId -> Edna (Maybe (WithId 'U.TargetId TargetResp))
getTargetById targetSqlId = do
  targetWithProjects <- targetsWithProjects $ Just targetSqlId
  case targetWithProjects of
    [] -> pure Nothing
    xs@((target, _) : _) -> pure $ Just $ targetToDomain targetSqlId target $ map snd xs

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

getTargetByName :: Text -> Edna (Maybe TargetRec)
getTargetByName name = runSelectReturningOne' $ select $ do
  targets <- all_ $ esTarget ednaSchema
  guard_ (LDB.tName targets ==. val_ name)
  pure targets

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

getCompoundById :: CompoundId -> Edna (Maybe CompoundRec)
getCompoundById (SqlId compoundId) = runSelectReturningOne' $ select $ do
  compounds <- all_ $ esCompound ednaSchema
  guard_ (cCompoundId compounds ==. val_ (SqlSerial compoundId))
  pure compounds

getCompounds :: Edna [CompoundRec]
getCompounds = runSelectReturningList' $ select $ all_ $ esCompound ednaSchema

editCompoundChemSoft :: CompoundId -> Text -> Edna ()
editCompoundChemSoft (SqlId compoundId) link = runUpdate' $ update (esCompound ednaSchema)
  (\c -> cChemsoftLink c <-. val_ (Just link))
  (\c -> cCompoundId c ==. val_ (SqlSerial compoundId))

getCompoundByName :: Text -> Edna (Maybe CompoundRec)
getCompoundByName name = runSelectReturningOne' $ select $ do
  compounds <- all_ $ esCompound ednaSchema
  guard_ (LDB.cName compounds ==. val_ name)
  pure compounds

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

getMethodologyById :: SqlId 'MethodologyId -> Edna (Maybe TestMethodologyRec)
getMethodologyById (SqlId methodologyId) = runSelectReturningOne' $
  lookup_ (esTestMethodology ednaSchema) $ TestMethodologyId $ SqlSerial methodologyId

getMethodologyByName :: Text -> Edna (Maybe TestMethodologyRec)
getMethodologyByName name = runSelectReturningOne' $ select $ do
  methodologies <- all_ $ esTestMethodology ednaSchema
  guard_ (LDB.tmName methodologies ==. val_ name)
  pure methodologies

getMethodologies :: Edna [TestMethodologyRec]
getMethodologies = runSelectReturningList' $ select $ all_ $ esTestMethodology ednaSchema

insertMethodology :: MethodologyReqResp -> Edna TestMethodologyRec
insertMethodology MethodologyReqResp{..} = runInsertReturningOne' $
  insert (esTestMethodology ednaSchema) $ insertExpressions
    [ TestMethodologyRec
      { tmTestMethodologyId = default_
      , tmName = val_ mrpName
      , tmDescription = val_ mrpDescription
      , tmConfluenceLink = val_ $ renderURI <$> mrpConfluence
      }
    ]

updateMethodology :: SqlId 'MethodologyId -> MethodologyReqResp -> Edna ()
updateMethodology (SqlId methodologyId) MethodologyReqResp{..} =
  runUpdate' $ update (esTestMethodology ednaSchema)
    (\tm -> mconcat
      [ LDB.tmName tm <-. val_ mrpName
      , LDB.tmDescription tm <-. val_ mrpDescription
      , LDB.tmConfluenceLink tm <-. val_ (renderURI <$> mrpConfluence)])
    (\tm -> tmTestMethodologyId tm ==. val_ (SqlSerial methodologyId))

deleteMethodology :: SqlId 'MethodologyId -> Edna ()
deleteMethodology (SqlId methodologyId) =
  runDelete' $ delete (esTestMethodology ednaSchema) $
  \m -> tmTestMethodologyId m ==. val_ (SqlSerial methodologyId)

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
  , prCreationDate = pCreationDate
  , prLastUpdate = pLastUpdate
  , prCompoundNames = mapMaybe (fmap cName) compounds
  }

getProjectById :: ProjectId -> Edna (Maybe ProjectRec)
getProjectById (SqlId projectId) = runSelectReturningOne' $
  lookup_ (esProject ednaSchema) $ LDB.ProjectId $ SqlSerial projectId

getProjectByName :: Text -> Edna (Maybe ProjectRec)
getProjectByName name = runSelectReturningOne' $ select $ do
  projects <- all_ $ esProject ednaSchema
  guard_ (pName projects ==. val_ name)
  pure projects

getProjectWithCompoundsById :: ProjectId -> Edna (Maybe (WithId 'U.ProjectId ProjectResp))
getProjectWithCompoundsById projectSqlId = do
  projectWithCompounds <- projectsWithCompounds $ Just projectSqlId
  case projectWithCompounds of
    [] -> pure Nothing
    xs@((project, _) : _) -> pure $ Just $ projectToDomain projectSqlId project $ map snd xs

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

updateProject :: ProjectId -> ProjectReq -> Edna ()
updateProject (SqlId projectId) ProjectReq{..} =
  runUpdate' $ update (esProject ednaSchema)
    (\p -> mconcat
      [ LDB.pName p <-. val_ prqName
      , LDB.pDescription p <-. val_ prqDescription
      , LDB.pLastUpdate p <-. default_])
    (\p -> pProjectId p ==. val_ (SqlSerial projectId))
