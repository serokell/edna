module Edna.Library.DB.Schema
  ( ProjectT (..)
  , ProjectRec

  ,TestMethodologyT (..)
  , TestMethodologyRec

  , TargetRec
  , TargetT (..)

  , CompoundT (..)
  , CompoundRec

  , PrimaryKey (..)
  ) where

import Universum

import Data.Time (LocalTime)
import Database.Beam.Backend.SQL.BeamExtensions (SqlSerial(..))
import Database.Beam.Schema (Beamable, C, Nullable, Table(..))

--------------------------
-- Project
--------------------------

data ProjectT f = ProjectRec
  { pProjectId :: C f (SqlSerial Word32)
  , pName :: C f Text
  , pDescription :: C (Nullable f) Text
  , pCreationDate :: C f LocalTime
  , pLastUpdate :: C f LocalTime
  } deriving stock Generic
    deriving anyclass Beamable

type ProjectRec = ProjectT Identity

deriving stock instance Show ProjectRec
deriving stock instance Eq ProjectRec

instance Table ProjectT where
  data PrimaryKey ProjectT f = ProjectId (C f (SqlSerial Word32))
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = ProjectId . pProjectId

deriving stock instance Show (PrimaryKey ProjectT Identity)
deriving stock instance Eq (PrimaryKey ProjectT Identity)

--------------------------
-- Test methodology
--------------------------

data TestMethodologyT f = TestMethodologyRec
  { tmTestMethodologyId :: C f (SqlSerial Word32)
  , tmName :: C f Text
  , tmDescription :: C (Nullable f) Text
  , tmConfluenceLink :: C (Nullable f) Text
  } deriving stock Generic
    deriving anyclass Beamable

type TestMethodologyRec = TestMethodologyT Identity

deriving stock instance Show TestMethodologyRec
deriving stock instance Eq TestMethodologyRec

instance Table TestMethodologyT where
  data PrimaryKey TestMethodologyT f = TestMethodologyId (C f (SqlSerial Word32))
    deriving stock (Generic)
    deriving anyclass Beamable
  primaryKey = TestMethodologyId . tmTestMethodologyId

deriving stock instance Show (PrimaryKey TestMethodologyT Identity)
deriving stock instance Eq (PrimaryKey TestMethodologyT Identity)

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
