-- | Random data generators for Edna's types.
--
-- We are using @hedgehog@ by default because it's superior to @QuickCheck@.
-- However, @validateEveryToJSON@ requires @Arbitrary@ instances, so we also
-- define them.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Gen
  (
    -- * Hedgehog
    genSqlId
  , genWithId
  , genStubSortBy
  , genNameAndId
  , genFileSummaryItem
  , genProjectReq
  , genProjectResp
  , genMethodologyReqResp
  , genCompoundResp
  , genTargetResp
  , genExperimentsResp
  , genExperimentResp
  , genSubExperimentResp
  , genMeasurementResp
  , genName
  , genURI
  , genDescription
  , genFileContents
  , genFileMetadata
  , genTargetMeasurements
  , genMeasurement
  , genLocalTime
  , genByteString
  , genDoubleSmallPrec
  , genUTCTime
  ) where

import Universum

import qualified Data.ByteString.Lazy as BL
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as HQC
import qualified Hedgehog.Range as Range

import Data.Time (LocalTime(..), UTCTime, fromGregorian, secondsToDiffTime, timeToTimeOfDay)
import Hedgehog (MonadGen)
import Lens.Micro (at, (?~))
import Network.URI (URIAuth(..), nullURI)
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Hedgehog (hedgehog)

import Edna.Dashboard.Web.Types
import Edna.ExperimentReader.Types
  (FileContents(..), FileMetadata(..), Measurement(..), TargetMeasurements(..))
import Edna.Library.Web.Types
  (CompoundResp(..), MethodologyReqResp(..), ProjectReq(..), ProjectResp(..), TargetResp(..))
import Edna.Upload.Web.API (ExperimentalMeasurement(..))
import Edna.Upload.Web.Types (FileSummary(..), FileSummaryItem(..), NameAndId(..))
import Edna.Util (SqlId(..), localToUTC)
import Edna.Web.Types

----------------
-- Heddgehog
----------------

genSqlId :: MonadGen m => m (SqlId t)
genSqlId = SqlId <$> Gen.integral (Range.constant 0 100)

genWithId :: MonadGen m => m t -> m (WithId k t)
genWithId genT = WithId <$> genSqlId <*> genT

genStubSortBy :: MonadGen m => m StubSortBy
genStubSortBy = Gen.element [SortByName, SortBySomething]

genName :: MonadGen m => m Text
genName = Gen.text (Range.linear 1 30) Gen.unicode

genDescription :: MonadGen m => m Text
genDescription = Gen.text (Range.linear 5 200) Gen.unicode

genURI :: forall m. MonadGen m => m URI
genURI = Gen.choice . (:[pure nullURI]) $ do
  uriScheme <- Gen.element ["http:", "ftp:"]
  uriAuthority <- Gen.maybe genURIAuth
  uriPath <- Gen.element ["/aaa", "/bbb", "/ccc"]
  uriQuery <- Gen.element ["?query", "?foo"]
  uriFragment <- Gen.element ["#frag", "#bar"]
  return URI {..}
  where
    -- doesn't matter for our needs
    genURIAuth :: m URIAuth
    genURIAuth = pure URIAuth
      { uriUserInfo = "edna@"
      , uriRegName = "www.leningrad.spb.ru"
      , uriPort = ":42"
      }

genNameAndId :: MonadGen m => m (NameAndId x)
genNameAndId = NameAndId <$> genName <*> Gen.maybe genSqlId

genFileSummaryItem :: MonadGen m => m FileSummaryItem
genFileSummaryItem = do
  compounds <- Gen.list (Range.linear 0 10) genNameAndId
  FileSummaryItem <$> genNameAndId <*> pure compounds

genProjectReq :: MonadGen m => m ProjectReq
genProjectReq = ProjectReq <$> genName <*> Gen.maybe genDescription

genProjectResp :: MonadGen m => m ProjectResp
genProjectResp = do
  prName <- genName
  prDescription <- Gen.maybe genDescription
  prCreationDate <- genUTCTime
  prLastUpdate <- genUTCTime
  prCompoundNames <- Gen.list (Range.linear 0 10) genName
  return ProjectResp {..}

genMethodologyReqResp :: MonadGen m => m MethodologyReqResp
genMethodologyReqResp = do
  mrpName <- genName
  mrpDescription <- Gen.maybe genDescription
  mrpConfluence <- Gen.maybe genURI
  return MethodologyReqResp {..}

genCompoundResp :: MonadGen m => m CompoundResp
genCompoundResp = do
  crName <- genName
  crChemSoft <- Gen.maybe genURI
  crAdditionDate <- genUTCTime
  return CompoundResp {..}

genTargetResp :: MonadGen m => m TargetResp
genTargetResp = do
  trName <- genName
  trProjects <- Gen.list (Range.linear 0 5) genName
  trAdditionDate <- genUTCTime
  return TargetResp {..}

genExperimentsResp :: MonadGen m => m ExperimentsResp
genExperimentsResp =
  ExperimentsResp
  <$> Gen.list (Range.linear 0 5) (genWithId genExperimentResp)
  <*> Gen.maybe genDoubleSmallPrec

genExperimentResp :: MonadGen m => m ExperimentResp
genExperimentResp = do
  erProject <- genSqlId
  erCompound <- genSqlId
  erTarget <- genSqlId
  erMethodology <- genSqlId
  erUploadDate <- genUTCTime
  erSubExperiments <- Gen.list (Range.linear 0 5) genSqlId
  return ExperimentResp {..}

genSubExperimentResp :: MonadGen m => m SubExperimentResp
genSubExperimentResp = do
  serName <- genName
  serIsDefault <- Gen.bool
  serIsSuspicious <- Gen.bool
  serIC50 <- genDoubleSmallPrec
  return SubExperimentResp {..}

genMeasurementResp :: MonadGen m => m MeasurementResp
genMeasurementResp = do
  mrConcentration <- genDoubleSmallPrec
  mrSignal <- genDoubleSmallPrec
  return MeasurementResp {..}

genFileContents :: MonadGen m => m Text -> m Text -> m FileContents
genFileContents genTargetName genCompoundName = do
  fcMeasurements <- genFileMeasurements genTargetName genCompoundName
  -- Metadata entities are similar to description items in some sense.
  fcMetadata <- genFileMetadata
  return FileContents {..}

genFileMetadata :: MonadGen m => m FileMetadata
genFileMetadata = FileMetadata <$> Gen.list (Range.constant 0 50) genDescription

-- Common logic of 'genFileMeasurements' and 'genTargetMeasurements'.
genHashMap ::
  forall m k v. (MonadGen m, Text ~ k) =>
  m k -> Int -> m v -> m (HashMap k v)
genHashMap genK minSize genV = do
  names <- Gen.set (Range.linear minSize 10) genK
  let
    step :: HashMap k v -> k -> m (HashMap k v)
    step acc name = do
      v <- genV
      return $ acc & at name ?~ v

  foldM step mempty names

genFileMeasurements ::
  MonadGen m => m Text -> m Text -> m (HashMap Text TargetMeasurements)
genFileMeasurements genTargetName genCompoundName =
  genHashMap genTargetName 0 (genTargetMeasurements genCompoundName)

genTargetMeasurements :: MonadGen m => m Text -> m TargetMeasurements
genTargetMeasurements genCompoundName =
  TargetMeasurements <$>
  genHashMap genCompoundName 1 (Gen.list (Range.linear 1 50) genMeasurement)

genMeasurement :: MonadGen m => m Measurement
genMeasurement = do
  mConcentration <- genDoubleSmallPrec
  mSignal <- genDoubleSmallPrec
  mIsOutlier <- Gen.bool
  return Measurement {..}

genLocalTime :: MonadGen m => m LocalTime
genLocalTime = do
    y <- toInteger <$> Gen.int (Range.constant 2000 2030)
    m <- Gen.int (Range.constant 1 12)
    d <- Gen.int (Range.constant 1 28)
    let day = fromGregorian y m d
    secs <- toInteger <$> Gen.int (Range.constant 0 86401)
    let timeOfDay = timeToTimeOfDay $ secondsToDiffTime secs
    pure $ LocalTime day timeOfDay

genUTCTime :: MonadGen m => m UTCTime
genUTCTime = localToUTC <$> genLocalTime

genByteString :: MonadGen m => m LByteString
genByteString = BL.fromStrict <$> Gen.bytes (Range.linear 5 200)

-- | Generate a 'Double' value with a small number of digits.
-- High precision numbers (presumably more than 15 digits) may cause issues
-- when you put them into PostgreSQL and then read.
-- In Edna we don't need very high precision because small imprecision won't
-- noticeably affect analysis outcome.
genDoubleSmallPrec :: MonadGen m => m Double
genDoubleSmallPrec = divideBy128 <$> Gen.word64 (Range.constant 0 300)
  where
    divideBy128 :: Word64 -> Double
    divideBy128 d = fromInteger (toInteger d) / 128

----------------
-- QuickCheck
----------------

-- This type is legacy and will likely be removed and replaced by something
-- else, so we are defining a dummy instance for now and now @hedgehog@
-- generator.
instance Arbitrary ExperimentalMeasurement where
  arbitrary = pure ExperimentalMeasurement
    { emCompoundId = "aa"
    , emTargetId = "qq"
    , emConcentration = 0
    , emSignal = 0
    , emOutlier = True
    }

deriving newtype instance Arbitrary (SqlId t)

instance Arbitrary t => Arbitrary (WithId k t) where
  arbitrary = hedgehog $ genWithId HQC.arbitrary

instance Arbitrary StubSortBy where
  arbitrary = hedgehog genStubSortBy

instance Arbitrary URI where
  arbitrary = hedgehog genURI

deriving newtype instance Arbitrary FileSummary

instance Arbitrary (NameAndId t) where
  arbitrary = hedgehog genNameAndId

instance Arbitrary FileSummaryItem where
  arbitrary = hedgehog genFileSummaryItem

instance Arbitrary ProjectReq where
  arbitrary = hedgehog genProjectReq

instance Arbitrary ProjectResp where
  arbitrary = hedgehog genProjectResp

instance Arbitrary MethodologyReqResp where
  arbitrary = hedgehog genMethodologyReqResp

instance Arbitrary CompoundResp where
  arbitrary = hedgehog genCompoundResp

instance Arbitrary TargetResp where
  arbitrary = hedgehog genTargetResp

instance Arbitrary ExperimentsResp where
  arbitrary = hedgehog genExperimentsResp

instance Arbitrary ExperimentResp where
  arbitrary = hedgehog genExperimentResp

instance Arbitrary SubExperimentResp where
  arbitrary = hedgehog genSubExperimentResp

instance Arbitrary MeasurementResp where
  arbitrary = hedgehog genMeasurementResp

-- Is needed for swagger tests
instance Arbitrary Text where
  arbitrary = hedgehog genName
