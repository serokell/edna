module Edna.Util
  ( NetworkAddress (..)
  , ConnString (..)
  , DatabaseInitOption (..)
  , SqlId (..)
  , IdType (..)
  , TargetId
  , CompoundId
  , MethodologyId
  , ProjectId
  , ExperimentFileId
  , ExperimentId
  , SubExperimentId
  , MeasurementId
  , ednaAesonWebOptions
  , ednaAesonConfigOptions
  , schemaOptions
  , gDeclareNamedSchema
  , gToParamSchema
  , justOrThrow
  , nothingOrThrow
  , ensureOrThrow
  , justOrError
  , fromSqlSerial
  , rightOrThrow
  , localToUTC
  ) where

import Universum

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), withText)
import qualified Data.Aeson as A
import qualified Data.Aeson.Casing as AC
import Data.Swagger (ToParamSchema, ToSchema)
import qualified Data.Swagger as S
import Data.Swagger.Declare (Declare)
import Data.Swagger.Internal (ParamSchema)
import Data.Swagger.Internal.ParamSchema (GToParamSchema, genericToParamSchema)
import qualified Data.Swagger.Internal.Schema as S
import Data.Swagger.Internal.TypeShape (GenericHasSimpleShape, GenericShape)
import Data.Swagger.SchemaOptions (SchemaOptions, fromAesonOptions)
import Data.Time (LocalTime, UTCTime, localTimeToUTC, utc)
import Database.Beam.Backend (SqlSerial(..))
import Fmt (Buildable(..), pretty, (+|), (|+))
import qualified GHC.Generics as G
import Servant (FromHttpApiData(..))
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read (Read(..), read)
import qualified Text.Show

----------------
-- ConnString
----------------

newtype ConnString = ConnString
  { unConnString :: ByteString
  } deriving stock (Show, Eq, Ord)

instance FromJSON ConnString where
  parseJSON = withText "ConnString" $ pure . ConnString . encodeUtf8

instance ToJSON ConnString where
  toJSON = String . decodeUtf8 . unConnString

instance Buildable ConnString where
  build (ConnString s) = build $ decodeUtf8 @Text s

----------------
-- NetworkAddress
----------------

-- | Datatype which contains info about socket network address
data NetworkAddress = NetworkAddress
  { naHost :: !Text
  , naPort :: !Word16
  } deriving stock (Eq, Ord, Generic)

instance Buildable NetworkAddress where
  build NetworkAddress {..} = ""+|naHost|+":"+|naPort|+""

instance Read NetworkAddress where
  readsPrec _ = ReadP.readP_to_S addrParser
    where
      addrParser = NetworkAddress
        <$> (parseHost <* ReadP.char ':')
        <*> parsePort
      parseHost = toText <$> ReadP.munch (/= ':')
      parsePort = ReadP.readS_to_P reads

instance Show NetworkAddress where
  show = pretty @NetworkAddress @String

instance FromJSON NetworkAddress where
  parseJSON = withText "NetworkAddress" $ pure . read . toString

instance ToJSON NetworkAddress where
  toJSON = String . pretty

----------------
-- DatabaseInitOption
----------------

data DatabaseInitOption = Enable | EnableWithDrop
  deriving stock (Generic, Show)

instance FromJSON DatabaseInitOption where
  parseJSON = withText "DatabaseInitOption" $ \o -> pure case o of
    "enable" -> Enable
    "enable-with-drop" -> EnableWithDrop
    _ -> error $ "Invalid config value: " <> show o

instance ToJSON DatabaseInitOption where
  toJSON Enable = "enable"
  toJSON EnableWithDrop = "enable-with-drop"

----------------
-- Aeson options
----------------

ednaAesonConfigOptions :: A.Options
ednaAesonConfigOptions = AC.aesonPrefix AC.trainCase

ednaAesonWebOptions :: A.Options
ednaAesonWebOptions = AC.aesonPrefix AC.camelCase

----------------
-- Swagger
----------------

-- | Schema generation options which match JSON generation options.
schemaOptions :: SchemaOptions
schemaOptions = fromAesonOptions ednaAesonWebOptions

-- | Default implementation of 'ToSchema' via Generics.
gDeclareNamedSchema
    :: ( Generic a
       , S.GToSchema (G.Rep a)
       , GenericHasSimpleShape a "genericDeclareNamedSchemaUnrestricted" (GenericShape (G.Rep a))
       )
    => Proxy a -> Declare (S.Definitions S.Schema) S.NamedSchema
gDeclareNamedSchema = S.genericDeclareNamedSchema schemaOptions

-- | Default implementation of 'ToParamSchema' via Generics.
gToParamSchema
    :: ( Generic a
       , GToParamSchema (G.Rep a)
       )
    => Proxy a -> ParamSchema t
gToParamSchema = genericToParamSchema schemaOptions

----------------
-- Util
----------------

justOrThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
justOrThrow e = maybe (throwM e) pure

justOrError :: Monad m => Text -> Maybe a -> m a
justOrError t = maybe (error t) pure

nothingOrThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m ()
nothingOrThrow e = maybe (pure ()) (\_ -> throwM e)

ensureOrThrow :: (MonadThrow  m, Exception e) => e -> Bool -> m ()
ensureOrThrow e b
  | b = pure ()
  | otherwise = throwM e

rightOrThrow :: (MonadThrow m, Exception e) => (b -> e) -> Either b a -> m a
rightOrThrow f = either (throwM . f) pure

localToUTC :: LocalTime -> UTCTime
localToUTC = localTimeToUTC utc

----------------
-- SqlId
----------------

-- | A simple wrapper over 'Word32'. At the data layer, we identify all entities
-- with numeric IDs and this data type corresponds to such an ID.
-- It has a phantom parameter type of a custom 'IdType' kind.
-- This parameter helps us statically distinguish IDs of different entities
-- (such as target, compound, etc.).
newtype SqlId (t :: IdType) = SqlId
  { unSqlId :: Word32
  } deriving stock (Generic, Show, Eq, Ord)
    deriving newtype (FromHttpApiData, FromJSON, ToJSON, ToSchema, Hashable)

instance Buildable (SqlId t) where
  build (SqlId n) = "ID#" <> build n

instance ToParamSchema (SqlId t) where
  toParamSchema = gToParamSchema

fromSqlSerial :: SqlSerial Word32 -> SqlId a
fromSqlSerial = SqlId . unSerial

-- | Kind used for phantom parameter in 'SqlId'.
data IdType
  = TargetId
  | CompoundId
  | MethodologyId
  | ProjectId
  | ExperimentFileId
  | ExperimentId
  | SubExperimentId
  | MeasurementId

type TargetId = SqlId 'TargetId
type CompoundId = SqlId 'CompoundId
type MethodologyId = SqlId 'MethodologyId
type ProjectId = SqlId 'ProjectId
type ExperimentFileId = SqlId 'ExperimentFileId
type ExperimentId = SqlId 'ExperimentId
type SubExperimentId = SqlId 'SubExperimentId
type MeasurementId = SqlId 'MeasurementId
