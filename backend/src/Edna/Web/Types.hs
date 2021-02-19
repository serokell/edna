module Edna.Web.Types
  ( ExperimentalMeasurement (..),
  )
where

import Universum

import Data.Aeson.TH (deriveJSON)

import Edna.Util (ednaAesonOptions)

data ExperimentalMeasurement = ExperimentalMeasurement
  { emCompoundId    :: Text,
    emConcentration :: Double,
    emSignal        :: Double
  } deriving stock (Generic)

deriveJSON ednaAesonOptions ''ExperimentalMeasurement
