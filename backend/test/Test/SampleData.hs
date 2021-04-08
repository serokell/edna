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
  , sampleMetadata
  , sampleDescription
  , sampleFileName
  , sampleFileBlob
  -- ↓ Violating style guide a bit, hopefully that's ok ↓
  , targetName1, targetName2, targetName3, targetName4
  , compoundName1, compoundName2, compoundName3, compoundName4, compoundName5
  , m1, m2, m3, m4, m5

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
import Edna.Library.Web.Types (MethodologyReqResp(..), ProjectReq(ProjectReq))
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
  [ (compoundName1, [m1, m2, m3])
  , (compoundName2, [m2, m4, m5])
  , (compoundName3, [m1, m3, m4])
  ]

targetMeasurements2 :: Map Text [Measurement]
targetMeasurements2 = Map.fromList
  [ (compoundName2, [m1, m2, m5])
  ]

targetMeasurements3 :: Map Text [Measurement]
targetMeasurements3 = Map.fromList
  [ (compoundName1, [m1, m5])
  , (compoundName4, [m3, m4, m5])
  ]

targetMeasurements4 :: Map Text [Measurement]
targetMeasurements4 = Map.fromList
  [ (compoundName1, [m1, m5])
  , (compoundName5, [m1, m2])
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
  Library.addMethodology (MethodologyReqResp methodologyName1
    methodologyDescription1 methodologyConfluence1) *>
  Library.addMethodology (MethodologyReqResp methodologyName2
    methodologyDescription2 methodologyConfluence2) $>
  ()

-- | Upload file with dummy metadata, bytes and description.
uploadFileTest :: ProjectId -> MethodologyId -> FileContents -> Edna ()
uploadFileTest projectId methodologyId =
  void . Upload.uploadFile' projectId methodologyId sampleDescription
  sampleFileName sampleFileBlob
