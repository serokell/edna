-- | Edna state for state machine testing.

module Test.SMT.State
  ( EdnaState (..)
  , esFiles
  , esTargetByName
  , esCompoundByName
  , initialState

  , Ensure
  , EdnaReader
  ) where

import Universum

import Hedgehog (Concrete, Test)
import Lens.Micro.Platform (makeLenses)

import Edna.ExperimentReader.Types (FileContents)
import Edna.Web.Types (Compound, SqlId, Target)

-- It's currently incomplete (just like everything in this file),
-- more data will be added later.
data EdnaState (v :: Type -> Type) = EdnaState
  { _esFiles :: HashMap Text FileContents
  -- ^ All files uploaded so far.
  , _esTargetByName :: HashMap Text (SqlId Target)
  -- ^ A way to quickly find target ID by its name.
  , _esCompoundByName :: HashMap Text (SqlId Compound)
  -- ^ A way to quickly find compound ID by its name.
  } deriving stock (Show, Eq)

makeLenses ''EdnaState

initialState :: EdnaState v
initialState = EdnaState
  { _esFiles = mempty
  , _esTargetByName = mempty
  , _esCompoundByName = mempty
  }

-- | Type of a predicate passed to @Ensure@ constructor.
type Ensure input output =
  EdnaState Concrete -> EdnaState Concrete -> input Concrete -> output -> Test ()

-- | Sometimes it's convenient to write @Ensure@ checks inside this monad.
type EdnaReader a = Reader (EdnaState Concrete) a
