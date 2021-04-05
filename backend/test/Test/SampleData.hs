-- | Sample data to be used in Edna tests.

module Test.SampleData
  ( sampleFile
  , sampleMetadata
  -- ↓ Violating style guide a bit ↓
  , m1, m2, m3, m4, m5
  ) where

import Universum

import qualified Data.Map.Strict as Map

import Edna.ExperimentReader.Types
  (FileContents(..), FileMetadata(..), Measurement(..), TargetMeasurements(..))

sampleFile :: FileContents
sampleFile = FileContents {..}
  where
    fcMeasurements = Map.fromList . map (second TargetMeasurements) $
      [ ("tar1", targetMeasurements1)
      , ("tar2", targetMeasurements2)
      , ("tar3", targetMeasurements3)
      ]
    fcMetadata = sampleMetadata

targetMeasurements1 :: Map Text [Measurement]
targetMeasurements1 = Map.fromList
  [ ("comp1", [m1, m2, m3])
  , ("comp2", [m2, m4, m5])
  , ("comp3", [m1, m3, m4])
  ]

targetMeasurements2 :: Map Text [Measurement]
targetMeasurements2 = Map.fromList
  [ ("comp2", [m1, m2, m5])
  ]

targetMeasurements3 :: Map Text [Measurement]
targetMeasurements3 = Map.fromList
  [ ("comp1", [m1, m5])
  , ("comp4", [m3, m4, m5])
  ]

m1, m2, m3, m4, m5 :: Measurement
m1 = Measurement
  { mConcentration = 1.5
  , mSignal = 200
  , mIsOutlier = False
  }
m2 = m1 {mConcentration = 3, mSignal = 400}
m3 = m1 {mConcentration = 6, mSignal = 600}
m4 = m1 {mConcentration = 12, mSignal = 800}
m5 = Measurement
  { mConcentration = 6
  , mSignal = 5000
  , mIsOutlier = True
  }

sampleMetadata :: FileMetadata
sampleMetadata = FileMetadata ["foo", "room", "mood"]
