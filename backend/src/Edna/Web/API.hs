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

import Edna.Web.Types (ExperimentalMeasurement)
import Servant.API ((:>), JSON, Post, Summary)
import Servant.API.Generic ((:-), AsApi, ToServant)
import Servant.Multipart (Mem, MultipartData(..), MultipartForm)

-- | API endpoints specification.
newtype EdnaEndpoints route = EdnaEndpoints
  { -- | Upload one experiment
    eeUploadExperiment :: route
      :- "experiment"
      :> Summary "Upload an EXCEL file describing one experiment"
      :> MultipartForm Mem (MultipartData Mem)
      :> Post '[JSON] [ExperimentalMeasurement]
  } deriving stock (Generic)

-- | API type specification.
type EdnaAPI =
  "api" :> ToServant EdnaEndpoints AsApi

ednaAPI :: Proxy EdnaAPI
ednaAPI = Proxy
