module Edna.Web.Types
  ( ExperimentalMeasurement (..),
  )
where

import Universum

import Data.Aeson.TH (deriveJSON)

import Edna.Util (ednaAesonWebOptions)

data ExperimentalMeasurement = ExperimentalMeasurement
  { emCompoundId :: Text
  , emTargetId :: Text
  , emConcentration :: Double
  , emSignal :: Double
  , emOutlier :: Bool
  } deriving stock (Generic)

deriveJSON ednaAesonWebOptions ''ExperimentalMeasurement
