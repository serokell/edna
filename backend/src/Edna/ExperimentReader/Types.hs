{-# OPTIONS_GHC -Wno-orphans #-}

module Edna.ExperimentReader.Types
  ( Parameter (..)
  , PlateUnit (..)
  , ParameterType (..)
  , Signal (..)
  , CellType (..)
  , PointYX (..)
  ) where

import Universum

import qualified GHC.Show as S

import Codec.Xlsx (CellValue(..))
import Fmt (Buildable(..), pretty, (+|), (|+))

newtype PointYX = PointYX (Int, Int)
  deriving stock (Show, Eq)

data ParameterType = Target | Compound

-- | Compound or target info
data Parameter = Parameter
  { pName :: Text -- ^ Parameter definition
  , pIndexes :: (Int, Int) -- ^ Range of parameter indexes on its axis
  } deriving stock Show

data PlateUnit = PlateUnit
  { tuTargets :: [Parameter]
  , tuCompounds :: [Parameter]
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
