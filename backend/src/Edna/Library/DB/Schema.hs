module Edna.Library.DB.Schema
  ( TargetRec
  , TargetT (..)
  
  , CompoundT (..)
  , CompoundRec
  ) where

import Universum

import Data.Time (LocalTime)
import Database.Beam.Backend.SQL.BeamExtensions (SqlSerial(..))
import Database.Beam.Schema (Beamable, C, Table(..), Nullable)

--------------------------
-- Target
--------------------------

data TargetT f = TargetRec
  { tTargetId :: C f (SqlSerial Word32)
  , tName :: C f Text
  , tCreationDate :: C f LocalTime
  } deriving stock Generic
    deriving anyclass Beamable

type TargetRec = TargetT Identity

deriving stock instance Show TargetRec
deriving stock instance Eq TargetRec

instance Table TargetT where
  data PrimaryKey TargetT f = TargetId (C f (SqlSerial Word32))
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = TargetId . tTargetId

deriving stock instance Show (PrimaryKey TargetT Identity)
deriving stock instance Eq (PrimaryKey TargetT Identity)

--------------------------
-- Compound
--------------------------

data CompoundT f = CompoundRec
  { cCompoundId :: C f (SqlSerial Word32)
  , cName :: C f Text
  , cCreationDate :: C f LocalTime
  , cChemsoftLink :: C (Nullable f) Text
  } deriving stock Generic
    deriving anyclass Beamable

type CompoundRec = CompoundT Identity

deriving stock instance Show CompoundRec
deriving stock instance Eq CompoundRec

instance Table CompoundT where
  data PrimaryKey CompoundT f = CompoundId (C f (SqlSerial Word32))
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = CompoundId . cCompoundId

deriving stock instance Show (PrimaryKey CompoundT Identity)
deriving stock instance Eq (PrimaryKey CompoundT Identity)
