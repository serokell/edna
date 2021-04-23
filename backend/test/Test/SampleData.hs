-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

-- | Sample data to be used in Edna tests.

module Test.SampleData
  ( -- * Projects
    projectName1
  , projectName2
  , projectDescription1
  , projectDescription2

  -- * Methodologies
  , methodologyName1
  , methodologyName2
  , methodologyDescription1
  , methodologyDescription2
  , methodologyConfluence1
  , methodologyConfluence2

  -- * Files
  , sampleFile
  , sampleFile2
  , autoOutlierFile
  , sampleMetadata
  , sampleDescription
  , sampleFileName
  , sampleFileBlob
  -- ↓ Violating style guide a bit, hopefully that's ok ↓
  , targetName1, targetName2, targetName3, targetName4
  , compoundName1, compoundName2, compoundName3, compoundName4, compoundName5
  , targetMeasurements1, targetMeasurements2, targetMeasurements3, targetMeasurements4
  , m1, m2, m3, m4, m5, m5Outlier, m6, m7, m8, m9, m10
  , autoOutlierMeasurements

  -- * Other
  , sampleURI
  , unknownSqlId

  -- * Actions
  , addSampleProjects
  , addSampleMethodologies
  , uploadFileTest
  ) where

import Universum
import qualified Universum.Unsafe as Unsafe

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map

import qualified Edna.Library.Service as Library
import qualified Edna.Upload.Service as Upload

import Edna.ExperimentReader.Types
  (FileContents(..), FileMetadata(..), Measurement(..), TargetMeasurements(..))
import Edna.Library.Web.Types (MethodologyReq(..), ProjectReq(ProjectReq))
import Edna.Setup (Edna)
import Edna.Util (MethodologyId, ProjectId, SqlId(..))
import Edna.Util.URI (URI, parseURI)

----------------
-- Projects
----------------

projectName1, projectName2 :: Text
projectName1 = "project1"
projectName2 = "project2"

projectDescription1, projectDescription2 :: Maybe Text
projectDescription1 = Just "First project"
projectDescription2 = Nothing

----------------
-- Methodologies
----------------

methodologyName1, methodologyName2 :: Text
methodologyName1 = "methodology1"
methodologyName2 = "methodology2"

methodologyDescription1, methodologyDescription2 :: Maybe Text
methodologyDescription1 = Just "First methodology"
methodologyDescription2 = Nothing

methodologyConfluence1, methodologyConfluence2 :: Maybe URI
methodologyConfluence1 = Just sampleURI
methodologyConfluence2 = Nothing

----------------
-- Files
----------------

sampleFile :: FileContents
sampleFile = mkFileContents
  [ (targetName1, targetMeasurements1)
  , (targetName2, targetMeasurements2)
  , (targetName3, targetMeasurements3)
  ]

sampleFile2 :: FileContents
sampleFile2 = mkFileContents
  [ (targetName1, targetMeasurements2)
  , (targetName4, targetMeasurements4)
  ]

-- This file contains one experiment where our analysis module finds one potential outlier
-- automatically.
autoOutlierFile :: FileContents
autoOutlierFile = mkFileContents
  [ (targetName1, one (compoundName1, autoOutlierMeasurements))
  ]

mkFileContents :: [(Text, Map Text [Measurement])] -> FileContents
mkFileContents items = FileContents {..}
  where
    fcMeasurements = Map.fromList . map (second TargetMeasurements) $ items
    fcMetadata = sampleMetadata

-- Targets are added in this order
targetName1, targetName2, targetName3, targetName4 :: Text
targetName1 = "tar1"
targetName2 = "tar2"
targetName3 = "tar3"
targetName4 = "tar4"

-- Compounds are added in this order
compoundName1, compoundName2, compoundName3, compoundName4, compoundName5 :: Text
compoundName1 = "comp1"
compoundName2 = "comp2"
compoundName3 = "comp3"
compoundName4 = "comp4"
compoundName5 = "comp5"

targetMeasurements1 :: Map Text [Measurement]
targetMeasurements1 = Map.fromList
  [ (compoundName1, [m1, m2, m3, m4, m5, m6, m7, m8, m9, m10])
  , (compoundName2, [m1, m2, m3, m4, m5Outlier, m6, m7, m8, m9, m10])
  , (compoundName3, [m1, m3, m4, m5, m7, m8, m9, m10])
  ]

targetMeasurements2 :: Map Text [Measurement]
targetMeasurements2 = Map.fromList
  [ (compoundName2, [m1, m2, m3, m4, m7, m8, m9])
  ]

targetMeasurements3 :: Map Text [Measurement]
targetMeasurements3 = Map.fromList
  [ (compoundName1, [m1, m3, m4, m5Outlier, m6, m7, m9, m10])
  , (compoundName4, [m1, m2, m4, m5Outlier, m7, m8, m9, m10])
  ]

targetMeasurements4 :: Map Text [Measurement]
targetMeasurements4 = Map.fromList
  [ (compoundName1, [m1, m3, m4, m5Outlier, m6, m7, m9, m10])
  , (compoundName5, [m1, m2, m4, m5Outlier, m7, m8, m9, m10])
  ]

m1, m2, m3, m4, m5, m5Outlier, m6, m7, m8, m9, m10 :: Measurement
m1 = Measurement
  { mConcentration = 10
  , mSignal = 1000
  , mIsOutlier = False
  }
m2 = m1 {mConcentration = 20, mSignal = 980}
m3 = m1 {mConcentration = 30, mSignal = 950}
m4 = m1 {mConcentration = 40, mSignal = 750}
m5 = m1 {mConcentration = 50, mSignal = 600}
m5Outlier = Measurement
  { mConcentration = 6
  , mSignal = 5000
  , mIsOutlier = True
  }
m6 = m1 {mConcentration = 60, mSignal = 400}
m7 = m1 {mConcentration = 70, mSignal = 250}
m8 = m1 {mConcentration = 80, mSignal = 150}
m9 = m1 {mConcentration = 90, mSignal = 120}
m10 = m1 {mConcentration = 100, mSignal = 100}

autoOutlierMeasurements :: [Measurement]
autoOutlierMeasurements = foldMap mkMeasurements
  [ (0.00508, [36144, 37086])
  , (0.01524, [36022, 36787])
  , (0.04572, [36248, 36362])
  , (0.13717, [36385, 36724])
  , (0.41152, [35417, 35992])
  , (1.2346, [35461,35479])
  , (3.7037, [33434,33917])
  , (11.111, [27498,27737])
  , (33.333, [16689,17041])
  , (100, [5973,6045])
  , (150, [1000])
  ]
  where
    mkMeasurements (c, signals) = flip map signals $ \s -> Measurement
      { mConcentration = c
      , mSignal = s
      , mIsOutlier = False
      }

sampleMetadata :: FileMetadata
sampleMetadata = FileMetadata ["foo", "room", "mood"]

sampleDescription :: Text
sampleDescription = "descr"

sampleFileName :: Text
sampleFileName = "file"

sampleFileBlob :: LByteString
sampleFileBlob = BSL.singleton 228

----------------
-- Miscellaneous
----------------

sampleURI :: HasCallStack => URI
sampleURI = Unsafe.fromJust (parseURI "localhost:1234/foo")

-- This ID is so big that we are quite sure nothing is identified by it
unknownSqlId :: SqlId anything
unknownSqlId = SqlId 1000

----------------
-- Actions
----------------

-- | Add sample projects from this module one by one. IDs are assigned from 1.
addSampleProjects :: Edna ()
addSampleProjects =
  Library.addProject (ProjectReq projectName1 projectDescription1) *>
  Library.addProject (ProjectReq projectName2 projectDescription2) $>
  ()

-- | Add sample methodologies from this module one by one. IDs are assigned from 1.
addSampleMethodologies :: Edna ()
addSampleMethodologies =
  Library.addMethodology (MethodologyReq methodologyName1
    methodologyDescription1 methodologyConfluence1) *>
  Library.addMethodology (MethodologyReq methodologyName2
    methodologyDescription2 methodologyConfluence2) $>
  ()

-- | Upload file with dummy metadata, bytes and description.
uploadFileTest :: ProjectId -> MethodologyId -> FileContents -> Edna ()
uploadFileTest projectId methodologyId =
  void . Upload.uploadFile' projectId methodologyId sampleDescription
  sampleFileName sampleFileBlob
