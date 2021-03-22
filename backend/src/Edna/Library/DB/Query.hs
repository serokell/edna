module Edna.Library.DB.Query
  ( getTargetById
  , getTargets
  ) where

import Universum

import qualified Data.List as L

import Database.Beam.Backend (SqlSerial(..))
import Database.Beam.Postgres (pgNubBy_)
import Database.Beam.Query
  (all_, asc_, cast_, filter_, int, just_, leftJoin_, orderBy_, select, val_, (==.))

import Edna.DB.Integration (runSelectReturningList')
import Edna.DB.Schema
  (EdnaSchema(..), ExperimentFileT(..), ExperimentT(..), ProjectRec, ProjectT(..), ednaSchema)
import Edna.Library.DB.Schema (TargetRec, TargetT(..))
import Edna.Library.Web.Types (TargetResp(..))
import Edna.Setup (Edna)
import Edna.Util (IdType(..), SqlId(..), TargetId)
import Edna.Web.Types (WithId(..))

targetToDomain :: TargetId -> TargetRec -> [Maybe ProjectRec] -> WithId 'TargetId TargetResp
targetToDomain targetSqlId TargetRec{..} projects = WithId targetSqlId $ TargetResp
  { trName = tName
  , trProjects = mapMaybe (fmap pName) projects
  , trCreationDate = tCreationDate
  }

getTargetById :: TargetId -> Edna (Maybe (WithId 'TargetId TargetResp))
getTargetById targetSqlId = do
  targetWithProjects <- targetsWithProjects $ Just targetSqlId
  case targetWithProjects of
    [] -> pure Nothing
    xs@((target, _) : _) -> pure $ Just $ targetToDomain targetSqlId target $ map snd xs

getTargets :: Edna [WithId 'TargetId TargetResp]
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
