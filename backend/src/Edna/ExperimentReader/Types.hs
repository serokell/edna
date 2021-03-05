module Edna.ExperimentReader.Types
  ( Parameter (..)
  , TabletUnit (..)
  , ParameterType (..)
  , Signal (..)
  , CellType (..)
  ) where

import Universum

import Codec.Xlsx (CellValue(..))
import Fmt (Buildable(..))

data ParameterType = Target | Compound

data Parameter = Parameter
  { pName :: Text
  , pIndexes :: (Int, Int)
  } deriving stock Show

data TabletUnit = TabletUnit
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

instance Buildable CellValue where
  build (CellText t) = build t
  build (CellDouble d) = build d
  build (CellBool b) = build b
  build (CellRich r) = build $ map (\rr -> show rr :: Text) r
  build (CellError e) = build $ (show e :: Text)
