module Test.DB.Gen
  ( genProjectRec
  , genTestMethodologyRec
  , genTargetRec
  , genCompoundRec
  , genExperimentFileRec
  , genExperimentRec
  , genMeasurementRec
  , genAnalysisMethodRec
  , genSubExperimentRec
  , genRemovedMeasurementsRec
  ) where

import Universum

import qualified Hedgehog.Internal.Gen as Gen

import Database.Beam.Backend.SQL.BeamExtensions (SqlSerial(..))
import Database.Beam.Postgres (PgJSON(..))
import Hedgehog.Range (constant)

import Edna.DB.Schema
  (AnalysisMethodRec, AnalysisMethodT(..), CompoundRec, CompoundT(..), ExperimentFileRec,
  ExperimentFileT(..), ExperimentRec, ExperimentT(..), MeasurementRec, MeasurementT(..),
  ProjectRec, ProjectT(..), RemovedMeasurementsRec, RemovedMeasurementsT(..), SubExperimentRec,
  SubExperimentT(..), TargetRec, TargetT(..), TestMethodologyRec, TestMethodologyT(..))
import Test.Gen
  (genByteString, genDescription, genDoubleSmallPrec, genFileMetadata, genLocalTime, genName,
  genURI)

genProjectRec :: Gen.MonadGen m => Word32 -> m ProjectRec
genProjectRec projectId = do
  pName <- genName
  pDescription <- Gen.maybe genDescription
  pCreationDate <- genLocalTime
  pLastUpdate <- genLocalTime
  pure ProjectRec {pProjectId = SqlSerial projectId, ..}

genTestMethodologyRec :: Gen.MonadGen m => Word32 -> m TestMethodologyRec
genTestMethodologyRec testMethodologyId = do
  tmName <- genName
  tmDescription <- Gen.maybe genName
  tmConfluenceLink <- show <<$>> Gen.maybe genURI
  pure TestMethodologyRec {tmTestMethodologyId = SqlSerial testMethodologyId, ..}

genTargetRec :: Gen.MonadGen m => Word32 -> m TargetRec
genTargetRec targetId = do
  tName <- genName
  tCreationDate <- genLocalTime
  pure TargetRec {tTargetId = SqlSerial targetId, ..}

genCompoundRec :: Gen.MonadGen m => Word32 -> m CompoundRec
genCompoundRec compoundId = do
  cName <- genName
  cCreationDate <- genLocalTime
  cChemsoftLink <- show <<$>> Gen.maybe genURI
  pure CompoundRec {cCompoundId = SqlSerial compoundId, ..}

genExperimentFileRec :: Gen.MonadGen m => Word32 -> Word32 -> Word32 -> m ExperimentFileRec
genExperimentFileRec experimentFileId efProjectId methodologyId = do
  efUploadDate <- genLocalTime
  efMeta <- PgJSON <<$>> Gen.maybe genFileMetadata
  efDescription <- genDescription
  efName <- genName
  efContents <- genByteString
  pure ExperimentFileRec
    { efExperimentFileId = SqlSerial experimentFileId
    , efMethodologyId = Just methodologyId
    , ..}

genExperimentRec :: Word32 -> Word32 -> Word32 -> Word32 -> ExperimentRec
genExperimentRec experimentId eExperimentFileId eCompoundId eTargetId = ExperimentRec
  { eExperimentId = SqlSerial experimentId
  , ..}

genMeasurementRec :: Gen.MonadGen m => Word32 -> Word32 -> m MeasurementRec
genMeasurementRec measurementId mExperimentId = do
  mConcentration <- genDoubleSmallPrec
  mSignal <- genDoubleSmallPrec
  mIsOutlier <- Gen.bool
  pure MeasurementRec {mMeasurementId = SqlSerial measurementId, ..}

genAnalysisMethodRec :: Gen.MonadGen m => Word32 -> m AnalysisMethodRec
genAnalysisMethodRec analysisMethodId = do
  amDescription <- Gen.maybe genDescription
  amParameters <- PgJSON <<$>> Gen.maybe (pure ())
  pure AnalysisMethodRec {amAnalysisMethodId = SqlSerial analysisMethodId, ..}

genSubExperimentRec :: Gen.MonadGen m => Word32 -> Word32 -> Word32 -> m SubExperimentRec
genSubExperimentRec subExperimentId seAnalysisMethodId seExperimentId = do
  seIsSuspicious <- Gen.bool
  seResult <- PgJSON <$> Gen.double (constant 0 100)
  pure SubExperimentRec {seSubExperimentId = SqlSerial subExperimentId, ..}

genRemovedMeasurementsRec :: Word32 -> Word32 -> RemovedMeasurementsRec
genRemovedMeasurementsRec rmSubExperimentId rmMeasurementId = RemovedMeasurementsRec {..}
