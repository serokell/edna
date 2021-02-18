module Edna.Web.Types
  ( ExperimentalMeasurement (..),
  )
where

import Universum

import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveJSON)

data ExperimentalMeasurement = ExperimentalMeasurement
  { emCompoundId    :: Text,
    emConcentration :: Double,
    emSignal        :: Double
  } deriving stock (Generic)

deriveJSON defaultOptions ''ExperimentalMeasurement
