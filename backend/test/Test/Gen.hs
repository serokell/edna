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
  , genWithExtra
  , genStubSortBy
  , genFileUploadReq
  , genFileSummaryItem
  , genProject
  , genProjectExtra
  , genTestMethodology
  , genCompound
  , genTarget
  ) where

import Universum

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.QuickCheck as HQC
import qualified Hedgehog.Range as Range

import Hedgehog (MonadGen)
import Network.URI (URIAuth(..))
import Test.QuickCheck (Arbitrary(..))
import Test.QuickCheck.Hedgehog (hedgehog)

import Edna.Upload.API (ExperimentalMeasurement(..), FileUploadReq(..))
import Edna.Web.Types

----------------
-- Heddgehog
----------------

genSqlId :: MonadGen m => m (SqlId t)
genSqlId = SqlId <$> Gen.integral (Range.constant 0 100)

genWithId :: MonadGen m => m t -> m (WithId t)
genWithId genT = WithId <$> genSqlId <*> genT

genWithExtra :: MonadGen m => m t -> m e -> m (WithExtra t e)
genWithExtra genT genE = WithExtra <$> genSqlId <*> genT <*> genE

genStubSortBy :: MonadGen m => m StubSortBy
genStubSortBy = Gen.element [SortByName, SortBySomething]

genName :: MonadGen m => m Text
genName = Gen.text (Range.linear 1 30) Gen.unicode

genDescription :: MonadGen m => m Text
genDescription = Gen.text (Range.linear 5 200) Gen.unicode

genURI :: forall m. MonadGen m => m URI
genURI = do
  uriScheme <- Gen.element ["http:", "ftp:"]
  uriAuthority <- Gen.maybe genURIAuth
  uriPath <- Gen.element ["", "aaa", "bbb", "ccc"]
  uriQuery <- Gen.element ["", "query", "foo"]
  uriFragment <- Gen.element ["", "frag", "bar"]
  return URI {..}
  where
    -- doesn't matter for our needs
    genURIAuth :: m URIAuth
    genURIAuth = pure URIAuth
      { uriUserInfo = "edna"
      , uriRegName = "www.leningrad.spb.ru"
      , uriPort = ":42"
      }

genFileUploadReq :: MonadGen m => m FileUploadReq
genFileUploadReq = FileUploadReq <$> genSqlId <*> genSqlId <*> genDescription

genFileSummaryItem :: MonadGen m => m FileSummaryItem
genFileSummaryItem = do
  let
    genIdOrName :: MonadGen m => m (Either (SqlId x) Text)
    genIdOrName = Gen.either genSqlId genName
  compounds <- Gen.list (Range.linear 0 10) genIdOrName
  FileSummaryItem <$> genIdOrName <*> pure compounds

genProject :: MonadGen m => m Project
genProject = Project <$> genName <*> genDescription

genProjectExtra :: MonadGen m => m ProjectExtra
genProjectExtra = do
  peCreationDate <- Gen.integral (Range.constant 0 1000)
  peLastUpdate <- Gen.integral (Range.constant 1000 10000)
  peCompoundNames <- Gen.list (Range.linear 0 10) genName
  return ProjectExtra {..}

genTestMethodology :: MonadGen m => m TestMethodology
genTestMethodology = do
  tmName <- genName
  tmDescription <- genDescription
  tmConfluence <- genURI
  return TestMethodology {..}

genCompound :: MonadGen m => m Compound
genCompound = do
  cName <- genName
  cChemSoft <- Gen.maybe genURI
  cAdditionDate <- Gen.integral (Range.constant 0 1000)
  return Compound {..}

genTarget :: MonadGen m => m Target
genTarget = do
  tName <- genName
  tProjects <- Gen.list (Range.linear 0 5) genName
  tCreationDate <- Gen.integral (Range.constant 0 1000)
  return Target {..}


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

instance Arbitrary t => Arbitrary (WithId t) where
  arbitrary = hedgehog $ genWithId HQC.arbitrary

instance (Arbitrary t, Arbitrary e) => Arbitrary (WithExtra t e) where
  arbitrary = hedgehog $ genWithExtra HQC.arbitrary HQC.arbitrary

instance Arbitrary StubSortBy where
  arbitrary = hedgehog genStubSortBy

instance Arbitrary URI where
  arbitrary = hedgehog genURI

instance Arbitrary FileUploadReq where
  arbitrary = hedgehog genFileUploadReq

deriving newtype instance Arbitrary FileSummary

instance Arbitrary FileSummaryItem where
  arbitrary = hedgehog genFileSummaryItem

instance Arbitrary Project where
  arbitrary = hedgehog genProject

instance Arbitrary ProjectExtra where
  arbitrary = hedgehog genProjectExtra

instance Arbitrary TestMethodology where
  arbitrary = hedgehog genTestMethodology

instance Arbitrary Compound where
  arbitrary = hedgehog genCompound

instance Arbitrary Target where
  arbitrary = hedgehog genTarget
