module Edna.Web.Types
  ( ExperimentalMeasurement (..),
  )
where

import Universum

data ExperimentalMeasurement = ExperimentalMeasurement
  { emCompoundId    :: Text,
    emConcentration :: Double,
    emSignal        :: Double
  } deriving stock (Generic)
