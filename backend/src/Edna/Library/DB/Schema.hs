module Edna.Library.DB.Schema
  ( TargetRec
  , TargetT (..)
  ) where

import Universum

import Data.Time (LocalTime)
import Database.Beam.Backend.SQL.BeamExtensions (SqlSerial(..))
import Database.Beam.Schema (Beamable, C, Table(..))

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
