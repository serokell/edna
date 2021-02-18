{-# LANGUAGE DataKinds #-}

{-|
Servant type-level specification for Edna API.
-}
module Edna.Web.API
     ( EdnaAPI
     , EdnaEndpoints (..)
     , ednaAPI
     ) where

import Universum

import Servant.API ((:>), JSON, Summary, Post)
import Servant.API.Generic ((:-), AsApi, ToServant)
import Servant.Multipart (MultipartData (..), MultipartForm, Mem)
import Edna.Web.Types (ExperimentalMeasurement)

-- | API endpoints specification.
data EdnaEndpoints route = EdnaEndpoints
  { -- | Upload one experiment
    eeUploadExperiment :: route
      :- "experiment"
      :> Summary "Upload an EXCEL file describing one experiment"
      :> MultipartForm Mem (MultipartData Mem)
      :> Post '[JSON] [ExperimentalMeasurement]
  } deriving stock (Generic)

-- | API type specification.
type EdnaAPI =
  "api" :> "v1" :> ToServant EdnaEndpoints AsApi

ednaAPI :: Proxy EdnaAPI
ednaAPI = Proxy
