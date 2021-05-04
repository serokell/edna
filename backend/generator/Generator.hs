-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Generator
  ( GeneratorSpec (..)
  , generatorDetails
  , generateAndSave
  ) where

import Universum

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Control.Exception.Safe (throwString)
import Options.Applicative.Help.Pretty (Doc, indent, linebreak)

import qualified Edna.Library.Service as Library
import qualified Edna.Upload.Service as Upload

import Edna.ExperimentReader.Types
  (FileContents(..), FileMetadata(..), Measurement(..), TargetMeasurements(..))
import Edna.Init (initEdna)
import Edna.Library.Error (LibraryError(..))
import Edna.Library.Web.Types (MethodologyReq(..), ProjectReq(..))
import Edna.Logging (logMessage)
import Edna.Setup (Edna)
import Edna.Util (SqlId(..))

-- | Underlying numeric type used in 'SqlId'. We use it as an index in most
-- cases here.
type Idx = Word32

-- | Specification for data generator: determines what data will be generated.
--
-- 'Word' seems to be the most appropriate type for indices, but here we use
-- 'Word32' aka 'Idx' in most cases because it's convenient to wrap into 'SqlId'.
-- We don't use it for measurements because here we don't construct measurement
-- IDs and add them to other indices.
data GeneratorSpec = GeneratorSpec
  { gsProjectNum :: Idx
  -- ^ How many projects to generate.
  , gsMethodologyNum :: Idx
  -- ^ How many methodologies to generate.
  , gsCompoundNum :: Idx
  -- ^ How many compounds to put into one file.
  , gsTargetNum :: Idx
  -- ^ How many targets to put into one file.
  , gsMeasurementsNum :: Word
  -- ^ Number of measurements in one experiment.
  , gsStrict :: Bool
  -- ^ Whether to fail in case of duplicates.
  }

-- | Detailed description of data generation procedure.
generatorDetails :: Doc
generatorDetails = mconcat
  [ "First we generate the requested number of projects and methodologies."
  , linebreak
  , "Then for each project and methodology a file is uploaded."
  , linebreak
  , "It has `--compounds` compounds and `--targets` targets, one experiment for each."
  , linebreak
  , "Each project has different compounds and targets."
  , linebreak
  , "The total number of entities is:"
  , linebreak
  , indent 2 $ mconcat
    [ "• `p` projects"
    , linebreak
    , "• `m` methodologies"
    , linebreak
    , "• `p * c` compounds and `p * t` targets"
    , linebreak
    , "• `p * m * c * t` experiments"
    ]
  ]

-- | Generate some dummy data and save it to DB according to the provided spec.
generateAndSave :: GeneratorSpec -> Edna ()
generateAndSave spec@GeneratorSpec {..} = do
  initEdna

  unless (gsCompoundNum > 0 && gsTargetNum > 0) $
    throwString "Number of compounds and targets must be positive"

  mapM_ addProject [1 .. gsProjectNum]
  logMessage $ "Added " <> show gsProjectNum <> " projects"

  mapM_ addMethodology [1 .. gsMethodologyNum]
  logMessage $ "Added " <> show gsMethodologyNum <> " methodologies"

  addExperiments spec
  where
    addProject i = void (Library.addProject ProjectReq
      { prqName = "Project #" <> show i
      , prqDescription = show i <$ guard (even i)
      }) `catch` \case
        LEProjectNameExists {} | not gsStrict -> pass
        e -> throwM e

    addMethodology i = void (Library.addMethodology MethodologyReq
      { mrqName = "Methodology #" <> show i
      , mrqDescription = show i <$ guard (even i)
      , mrqConfluence = Nothing
      }) `catch` \case
        LEMethodologyNameExists {} | not gsStrict -> pass
        e -> throwM e

addExperiments :: GeneratorSpec -> Edna ()
addExperiments spec@GeneratorSpec {..} = do
  forM_ [1 .. gsProjectNum] $ \projectIdx -> do
    forM_ [1 .. gsMethodologyNum] $ \methodologyIdx ->
      addExperiment projectIdx methodologyIdx
    logMessage $ "Processed project " <> show projectIdx
  where
    addExperiment :: Idx -> Idx -> Edna ()
    addExperiment projectId methodologyId = do
      let description =
            Text.replicate 100 (show projectId) <>
            Text.replicate 100 (show methodologyId)
      let metadata = replicate 100 (Text.replicate 1000 "x")
      let fc = FileContents
            { fcMeasurements = fileMeasurements spec projectId methodologyId
            , fcMetadata = FileMetadata metadata
            }
      let blob = BSL.replicate 3e5 0x42
      void $ Upload.uploadFile' (SqlId projectId) (SqlId methodologyId)
        description "experiment.xlsx" blob fc

fileMeasurements :: GeneratorSpec -> Idx -> Idx -> Map Text TargetMeasurements
fileMeasurements GeneratorSpec {..} projectIdx methodologyIdx =
  Map.fromList $ flip map [0 .. gsTargetNum - 1] $ \targetIdx ->
    let
      targetIdx' = (projectIdx - 1) * gsTargetNum + targetIdx
      targetName = "TARGET number " <> show targetIdx'
    in (targetName,) . TargetMeasurements . Map.fromList $
      flip map [0 .. gsCompoundNum - 1] $ \compoundIdx ->
        let
          compoundIdx' = (projectIdx - 1) * gsCompoundNum + compoundIdx
          compoundName = "COMPOUND number " <> show compoundIdx'
        in (compoundName, measurements gsMeasurementsNum projectIdx methodologyIdx)

measurements :: Word -> Idx -> Idx -> [Measurement]
measurements n project methodology = map measurement [1 .. n]
  where
    -- 4PL is computed as @f(x) = d + (a - d) / (1 + (x / c)^b)@.
    -- Below we set @a@ to project index, @b@ to 1, @c@ to methodology index and @d@ to 0.
    -- We also add some small value @eps@.
    measurement i =
      let
        concentration = 200 / 2 ^ i
        signal = realToFrac project / (1 + (concentration / realToFrac methodology))
        signalWithEps = signal + -1 ^ i * eps i
      in Measurement
      { mConcentration = concentration
      , mSignal = signalWithEps
      , mIsOutlier = i == 5
      }
    eps i = realToFrac (project * methodology) / (1e2 * realToFrac i)
