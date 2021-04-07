-- | Dashboard-related part of API definition along with implementation.

module Edna.Dashboard.Web.API
  ( DashboardEndpoints (..)
  , DashboardAPI
  , dashboardEndpoints
  ) where

import Universum

import Servant (ReqBody)
import Servant.API (Capture, Delete, Get, JSON, NoContent, Post, Put, QueryParam, Summary, (:>))
import Servant.API.Generic (AsApi, ToServant, (:-))
import Servant.Server.Generic (AsServerT, genericServerT)

import Edna.Analysis.FourPL (Params4PL)
import Edna.Dashboard.Service
  (analyseNewSubExperiment, deleteSubExperiment, getExperiments, getMeasurements, getSubExperiment,
  makePrimarySubExperiment, newSubExperiment, setIsSuspiciousSubExperiment, setNameSubExperiment)
import Edna.Dashboard.Web.Types
import Edna.Setup (Edna)
import Edna.Util (CompoundId, IdType(..), ProjectId, SubExperimentId, TargetId)
import Edna.Web.Types (StubSortBy, WithId)

-- TODO: pagination and sorting are just stubs for now.

-- | Endpoints related to projects.
data DashboardEndpoints route = DashboardEndpoints
  { -- | Make a sub-experiment primary.
    deMakePrimarySubExp :: route
      :- "subExperiment"
      :> "primary"
      :> Summary "Make a sub-experiment primary."
      :> Capture "subExperimentId" SubExperimentId
      :> Post '[JSON] (WithId 'SubExperimentId SubExperimentResp)

  , -- | Update the name of a sub-experiment.
    deSetNameSubExp :: route
      :- "subExperiment"
      :> "name"
      :> Summary "Update the name of a sub-experiment."
      :> Capture "subExperimentId" SubExperimentId
      :> ReqBody '[JSON] Text
      :> Put '[JSON] (WithId 'SubExperimentId SubExperimentResp)

  , -- | Update @isSupicious@ flag of a sub-experiment.
    deSetIsSuspiciousSubExp :: route
      :- "subExperiment"
      :> "suspicious"
      :> Summary "Update 'isSupicious' flag of a sub-experiment."
      :> Capture "subExperimentId" SubExperimentId
      :> ReqBody '[JSON] Bool
      :> Put '[JSON] (WithId 'SubExperimentId SubExperimentResp)

  , -- | Delete a sub-experiment.
    deDeleteSubExp :: route
      :- "subExperiment"
      :> Summary "Delete a sub-experiment."
      :> Capture "subExperimentId" SubExperimentId
      :> Delete '[JSON] NoContent

  , -- | Create a new sub-experiment from existing one.
    deNewSubExp :: route
      :- "subExperiment"
      :> Summary "Create a new sub-experiment from existing one."
      :> Capture "subExperimentId" SubExperimentId
      :> "new"
      :> ReqBody '[JSON] NewSubExperimentReq
      :> Post '[JSON] (WithId 'SubExperimentId SubExperimentResp)

  , -- | Analyse a new sub-experiment that is not created yet.
    deAnalyseNewSubExp :: route
      :- "subExperiment"
      :> Summary "Analyse a new sub-experiment that is not created yet."
      :> Capture "subExperimentId" SubExperimentId
      :> "new"
      :> "analyse"
      :> ReqBody '[JSON] NewSubExperimentReq
      :> Post '[JSON] Params4PL

  , -- | Get known experiments with optional pagination and sorting
    deGetExperiments :: route
      :- "experiments"
      :> Summary "Get known experiments"
      :> QueryParam "projectId" ProjectId
      :> QueryParam "compoundId" CompoundId
      :> QueryParam "targetId" TargetId

      :> QueryParam "page" Word
      :> QueryParam "size" Word
      :> QueryParam "sortby" StubSortBy
      :> Get '[JSON] ExperimentsResp

  , -- | Get sub-experiment's (meta-)data by ID.
    deGetSubExperiment :: route
      :- "subExperiment"
      :> Summary "Get sub-experiment's (meta-)data by ID"
      :> Capture "subExperimentId" SubExperimentId
      :> Get '[JSON] (WithId 'SubExperimentId SubExperimentResp)

  , -- | Get sub-experiment's measurements by ID.
    deGetMeasurements :: route
      :- "subExperiment"
      :> Summary "Get sub-experiment's measurements by ID"
      :> Capture "subExperimentId" SubExperimentId
      :> "measurements"
      :> Get '[JSON] [WithId 'MeasurementId MeasurementResp]
  } deriving stock (Generic)

type DashboardAPI = ToServant DashboardEndpoints AsApi

dashboardEndpoints :: ToServant DashboardEndpoints (AsServerT Edna)
dashboardEndpoints = genericServerT DashboardEndpoints
  { deMakePrimarySubExp = makePrimarySubExperiment
  , deSetNameSubExp = setNameSubExperiment
  , deSetIsSuspiciousSubExp = setIsSuspiciousSubExperiment
  , deDeleteSubExp = deleteSubExperiment
  , deNewSubExp = newSubExperiment
  , deAnalyseNewSubExp = fmap snd ... analyseNewSubExperiment
  , deGetExperiments = \p c t _ _ _ -> getExperiments p c t
  , deGetSubExperiment = getSubExperiment
  , deGetMeasurements = getMeasurements
  }
