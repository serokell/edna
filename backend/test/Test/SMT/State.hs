-- | Edna state for state machine testing.

module Test.SMT.State
  ( EdnaState (..)
  , esTargetToName
  , esTargetByName
  , esCompoundToName
  , esCompoundByName
  , esProjects
  , esTestMethodologies
  , initialState

  , ProjectState (..)
  , psFiles
  , psProject

  , Ensure
  , EdnaReader
  ) where

import Universum

import Hedgehog (Concrete, Test)
import Lens.Micro.Platform (makeLenses)

import Edna.ExperimentReader.Types (FileContents)
import Edna.Library.Web.Types (MethodologyReq, ProjectReq)
import Edna.Util (CompoundId, MethodologyId, ProjectId, TargetId)

-- It's currently incomplete (just like everything in this file),
-- more data will be added later.
data EdnaState (v :: Type -> Type) = EdnaState
  { _esTargetToName :: HashMap TargetId Text
  -- ^ Names and IDs of all targets added so far.
  , _esTargetByName :: HashMap Text TargetId
  -- ^ A way to quickly find target ID by its name.
  , _esCompoundToName :: HashMap CompoundId Text
  -- ^ Names and IDs of all compounds added so far.
  , _esCompoundByName :: HashMap Text CompoundId
  -- ^ A way to quickly find compound ID by its name.
  , _esProjects :: HashMap ProjectId ProjectState
  -- ^ All projects added so far.
  , _esTestMethodologies :: HashMap MethodologyId MethodologyReq
  -- ^ All test methodologies added so far.
  } deriving stock (Show, Eq)

-- | Data stored for each project.
data ProjectState = ProjectState
  { _psFiles :: [(FileContents, MethodologyId)]
  , _psProject :: ProjectReq
  } deriving stock (Show, Eq)

makeLenses ''EdnaState
makeLenses ''ProjectState

initialState :: EdnaState v
initialState = EdnaState
  { _esTargetToName = mempty
  , _esTargetByName = mempty
  , _esCompoundToName = mempty
  , _esCompoundByName = mempty
  , _esProjects = mempty
  , _esTestMethodologies = mempty
  }

-- | Type of a predicate passed to @Ensure@ constructor.
type Ensure input output =
  EdnaState Concrete -> EdnaState Concrete -> input Concrete -> output -> Test ()

-- | Sometimes it's convenient to write @Ensure@ checks inside this monad.
type EdnaReader a = Reader (EdnaState Concrete) a
