-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

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
  , genNameAndId
  , genParams4PL
  , genFileSummaryItem
  , genProjectReq
  , genProjectResp
  , genMethodologyReq
  , genMethodologyResp
  , genCompoundResp
  , genTargetResp
  , genExperimentsResp
  , genExperimentResp
  , genSubExperimentResp
  , genMeasurementResp
  , genExperimentMetadata
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
import qualified Data.HashSet as HS
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as HQC
import qualified Hedgehog.Range as Range

import Data.Time (LocalTime(..), UTCTime, fromGregorian, secondsToDiffTime, timeToTimeOfDay)
import Hedgehog (MonadGen)
import Control.Lens (at, (?~))
import Network.URI (URIAuth(..), nullURI)
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Hedgehog (hedgehog)

import Edna.Analysis.FourPL (Params4PL(..))
import Edna.Dashboard.Web.Types
import Edna.ExperimentReader.Types
  (FileContents(..), FileMetadata(..), Measurement(..), TargetMeasurements(..))
import Edna.Library.Web.Types
  (CompoundResp(..), MethodologyReq(..), MethodologyResp(..), ProjectReq(..), ProjectResp(..),
  TargetResp(..))
import Edna.Upload.Web.Types (FileSummary(..), FileSummaryItem(..), NameAndId(..))
import Edna.Util (SqlId(..), localToUTC)
import Edna.Web.Types

import Test.Util (methodologyReqToResp)

----------------
-- Heddgehog
----------------

genSqlId :: MonadGen m => m (SqlId t)
genSqlId = SqlId <$> Gen.integral (Range.constant 0 100)

genWithId :: MonadGen m => m t -> m (WithId k t)
genWithId genT = WithId <$> genSqlId <*> genT

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

genParams4PL :: MonadGen m => m Params4PL
genParams4PL =
  Params4PL
  <$> genDoubleSmallPrec
  <*> genDoubleSmallPrec
  <*> genDoubleSmallPrec
  <*> genDoubleSmallPrec

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

genMethodologyReq :: MonadGen m => m MethodologyReq
genMethodologyReq = do
  mrqName <- genName
  mrqDescription <- Gen.maybe genDescription
  mrqConfluence <- Gen.maybe genURI
  return MethodologyReq {..}

genMethodologyResp :: MonadGen m => m MethodologyResp
genMethodologyResp =
  methodologyReqToResp <$> genMethodologyReq <*> Gen.list (Range.linear 0 5) genName

genCompoundResp :: MonadGen m => m CompoundResp
genCompoundResp = do
  crName <- genName
  crChemSoft <- Gen.maybe genURI
  crMde <- Gen.maybe genURI
  crAdditionDate <- genUTCTime
  return CompoundResp {..}

genTargetResp :: MonadGen m => m TargetResp
genTargetResp = do
  trName <- genName
  trProjects <- Gen.list (Range.linear 0 5) genName
  trAdditionDate <- genUTCTime
  return TargetResp {..}

genNewSubExperimentReq :: MonadGen m => m NewSubExperimentReq
genNewSubExperimentReq =
  NewSubExperimentReq
  <$> genName
  <*> (HS.fromList <$> Gen.list (Range.linear 0 5) genSqlId)

genExperimentsResp :: MonadGen m => m ExperimentsResp
genExperimentsResp =
  ExperimentsResp <$> Gen.list (Range.linear 0 5) (genWithId genExperimentResp)

genExperimentResp :: MonadGen m => m ExperimentResp
genExperimentResp = do
  erProject <- genSqlId
  erCompound <- (,) <$> genSqlId <*> genName
  erTarget <- (,) <$> genSqlId <*> genName
  erMethodology <- Gen.maybe $ (,) <$> genSqlId <*> genName
  erUploadDate <- genUTCTime
  erSubExperiments <- Gen.list (Range.linear 1 5) genSqlId
  erPrimarySubExperiment <- Gen.element erSubExperiments
  erPrimaryIC50 <- Gen.either genDescription genDoubleSmallPrec
  return ExperimentResp {..}

genSubExperimentResp :: MonadGen m => m SubExperimentResp
genSubExperimentResp = do
  serName <- genName
  serIsSuspicious <- Gen.bool
  serResult <- Gen.either genName genParams4PL
  return SubExperimentResp {..}

genMeasurementResp :: MonadGen m => m MeasurementResp
genMeasurementResp = do
  mrConcentration <- genDoubleSmallPrec
  mrSignal <- genDoubleSmallPrec
  mrIsEnabled <- Gen.bool
  return MeasurementResp {..}

genExperimentMetadata :: MonadGen m => m ExperimentMetadata
genExperimentMetadata = do
  emDescription <- genDescription
  FileMetadata emFileMetadata <- genFileMetadata
  return ExperimentMetadata {..}

genFileContents :: MonadGen m => m Text -> m Text -> m FileContents
genFileContents genTargetName genCompoundName = do
  fcMeasurements <- genFileMeasurements genTargetName genCompoundName
  -- Metadata entities are similar to description items in some sense.
  fcMetadata <- genFileMetadata
  return FileContents {..}

genFileMetadata :: MonadGen m => m FileMetadata
genFileMetadata = FileMetadata <$> Gen.list (Range.constant 0 50) genDescription

-- Common logic of 'genFileMeasurements' and 'genTargetMeasurements'.
genMap ::
  forall m k v. (MonadGen m, Text ~ k) =>
  m k -> Int -> m v -> m (Map k v)
genMap genK minSize genV = do
  names <- Gen.set (Range.linear minSize 10) genK
  let
    step :: Map k v -> k -> m (Map k v)
    step acc name = do
      v <- genV
      return $ acc & at name ?~ v

  foldM step mempty names

genFileMeasurements ::
  MonadGen m => m Text -> m Text -> m (Map Text TargetMeasurements)
genFileMeasurements genTargetName genCompoundName =
  genMap genTargetName 0 (genTargetMeasurements genCompoundName)

genTargetMeasurements :: MonadGen m => m Text -> m TargetMeasurements
genTargetMeasurements genCompoundName =
  TargetMeasurements <$>
  genMap genCompoundName 1 (Gen.list (Range.linear 1 50) genMeasurement)

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
    secs <- toInteger <$> Gen.int (Range.constant 0 86399)
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

deriving newtype instance Arbitrary (SqlId t)

instance Arbitrary t => Arbitrary (WithId k t) where
  arbitrary = hedgehog $ genWithId HQC.arbitrary

instance Arbitrary URI where
  arbitrary = hedgehog genURI

instance Arbitrary Params4PL where
  arbitrary = hedgehog genParams4PL

deriving newtype instance Arbitrary FileSummary

instance Arbitrary (NameAndId t) where
  arbitrary = hedgehog genNameAndId

instance Arbitrary FileSummaryItem where
  arbitrary = hedgehog genFileSummaryItem

instance Arbitrary ProjectReq where
  arbitrary = hedgehog genProjectReq

instance Arbitrary ProjectResp where
  arbitrary = hedgehog genProjectResp

instance Arbitrary MethodologyReq where
  arbitrary = hedgehog genMethodologyReq

instance Arbitrary MethodologyResp where
  arbitrary = hedgehog genMethodologyResp

instance Arbitrary CompoundResp where
  arbitrary = hedgehog genCompoundResp

instance Arbitrary TargetResp where
  arbitrary = hedgehog genTargetResp

instance Arbitrary NewSubExperimentReq where
  arbitrary = hedgehog genNewSubExperimentReq

instance Arbitrary ExperimentsResp where
  arbitrary = hedgehog genExperimentsResp

instance Arbitrary ExperimentResp where
  arbitrary = hedgehog genExperimentResp

instance Arbitrary SubExperimentResp where
  arbitrary = hedgehog genSubExperimentResp

instance Arbitrary MeasurementResp where
  arbitrary = hedgehog genMeasurementResp

instance Arbitrary ExperimentMetadata where
  arbitrary = hedgehog genExperimentMetadata

-- Is needed for swagger tests
instance Arbitrary Text where
  arbitrary = hedgehog genName
