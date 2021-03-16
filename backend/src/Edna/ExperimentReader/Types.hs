{-# OPTIONS_GHC -Wno-orphans #-}
-- https://github.com/serokell/universum/issues/208
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Edna.ExperimentReader.Types
  ( Measurement (..)
  , TargetMeasurements (..)
  , FileContents (..)

  , Parameter (..)
  , PlateUnit (..)
  , ParameterType (..)
  , Signal (..)
  , CellType (..)
  , PointYX (..)
  ) where

import Universum

import qualified Data.HashMap.Strict as HM
import qualified GHC.Show as S

import Codec.Xlsx (CellValue(..))
import Fmt (Buildable(..), pretty, (+|), (|+))

----------------
-- ExperimenterReader API types
----------------

-- | A single experimental measurement from an experiment data file.
data Measurement = Measurement
  { mConcentration :: Double
  -- ^ Concentration for which the signal is measured.
  , mSignal :: Double
  -- ^ Something that is measured.
  , mIsOutlier :: Bool
  -- ^ Whether this measurement was explicitly marked as outlier.
  } deriving stock (Show)

-- | All measurements for one target.
-- Keys are compound names, corresponding values are measurements for
-- this compound.
newtype TargetMeasurements = TargetMeasurements
  { unTargetMeasurements :: HashMap Text [Measurement]
  } deriving stock (Show)
    deriving newtype (Container)

instance Semigroup TargetMeasurements where
  TargetMeasurements tm1 <> TargetMeasurements tm2 =
    TargetMeasurements $ HM.unionWith mappend tm1 tm2

instance Monoid TargetMeasurements where
  mempty = TargetMeasurements mempty

-- | All data that we read from a single experiment data file.
data FileContents = FileContents
  { fcMeasurements :: HashMap Text TargetMeasurements
  -- ^ All measumerents in a file.
  -- Keys are target names, corresponding values are measurements for
  -- this target.
  , fcMetadata :: ()
  -- ^ Metadata stored in the file. It's currently ignored, will be implemented
  -- later.
  } deriving stock (Show)

----------------
-- Internal types
----------------

newtype PointYX = PointYX (Int, Int)
  deriving stock (Show, Eq)

data ParameterType = Target | Compound

-- | Compound or target info
data Parameter = Parameter
  { pName :: Text -- ^ Parameter definition
  , pIndexes :: (Int, Int) -- ^ Range of parameter indexes on its axis
  } deriving stock Show

data PlateUnit = PlateUnit
  { puTargets :: [Parameter]
  , puCompounds :: [Parameter]
  }

data Signal = Signal
  { sValue :: Double
  , sOutlier :: Bool
  }

data CellType :: Type -> Type where
 CText  :: CellType Text
 CDouble :: CellType Double
 CSignal :: CellType Signal

instance Buildable PointYX where
  build (PointYX (y, x)) = "{row=" +| y |+ ", column=" +| x |+ "}"

instance Buildable (CellType a) where
  build CText = "text"
  build CDouble = "double"
  build CSignal = "signal"

instance Show (CellType a) where
  show = pretty

instance Eq (CellType a) where
  (==) a b = (pretty a :: Text) == pretty b

instance Buildable CellValue where
  build (CellText t) = build t
  build (CellDouble d) = build d
  build (CellBool b) = build b
  build (CellRich r) = build $ map (\rr -> show rr :: Text) r
  build (CellError e) = build (show e :: Text)
