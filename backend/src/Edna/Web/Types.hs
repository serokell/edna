module Edna.Web.Types
  ( ExperimentalMeasurement (..),
  )
where

import Universum

import Data.Aeson.TH (deriveJSON)

import Edna.Util (ednaAesonWebOptions)

data ExperimentalMeasurement = ExperimentalMeasurement
  { emCompoundId    :: Text,
    emConcentration :: Double,
    emSignal        :: Double
  } deriving stock (Generic)

deriveJSON ednaAesonWebOptions ''ExperimentalMeasurement
