-- SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io>
--
-- SPDX-License-Identifier: AGPL-3.0-or-later

module Edna.Util
  ( NetworkAddress (..)
  , CompoundId
  , ConnString (..)
  , DatabaseInitOption (..)
  , ExperimentFileId
  , ExperimentId
  , Host
  , IdType (..)
  , MeasurementId
  , MethodologyId
  , Port
  , ProjectId
  , SqlId (..)
  , SubExperimentId
  , TargetId
  , buildFromJSON
  , ednaAesonConfigOptions
  , ednaAesonPythonOptions
  , ednaAesonWebOptions
  , ensureOrThrow
  , fromSqlSerial
  , gDeclareNamedSchema
  , gToParamSchema
  , justOrError
  , justOrThrow
  , localToUTC
  , nothingOrThrow
  , oneOrError
  , parseDatabaseInitOption
  , rightOrThrow
  , schemaOptions
  , uncurry3
  , logUnconditionally
  ) where

import Universum

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), encode, withText)
import qualified Data.Aeson as A
import qualified Data.Aeson.Casing as AC
import Data.OpenApi (ToParamSchema, ToSchema(..))
import qualified Data.OpenApi as O
import Data.OpenApi.Declare (Declare)
import qualified Data.OpenApi.Internal.ParamSchema as O
import qualified Data.OpenApi.Internal.Schema as O
import Data.OpenApi.SchemaOptions (SchemaOptions, fromAesonOptions)
import qualified Data.Text as T
import Data.Time (LocalTime, UTCTime, localTimeToUTC, utc)
import Database.Beam.Backend (SqlSerial(..))
import Fmt (Buildable(..), Builder, pretty, (+|), (|+))
import qualified GHC.Generics as G
import Servant (FromHttpApiData(..))
import Servant.Util.Combinators.Logging (ForResponseLog, buildForResponse)
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

instance Semigroup ConnString where
  ConnString a <> ConnString "" = ConnString a
  ConnString "" <> ConnString b = ConnString b
  ConnString a <> ConnString b = ConnString $ a <> " " <> b

instance Monoid ConnString where
  mempty = ConnString ""

----------------
-- NetworkAddress
----------------

-- | Datatype which contains info about socket network address
type Host = Text
type Port = Word16

data NetworkAddress = NetworkAddress
  { naHost :: !Host
  , naPort :: !Port
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

-- | Parse DatabaseInitOption
parseDatabaseInitOption :: e -> String -> Either e DatabaseInitOption
parseDatabaseInitOption err value =
  case value of
    "enable"           -> Right Enable
    "enable-with-drop" -> Right EnableWithDrop
    _                  -> Left err

instance FromJSON DatabaseInitOption where
  parseJSON = withText "DatabaseInitOption" $ \text ->
    let value = parseDatabaseInitOption Nothing $ T.unpack text
    in case value of
      Right x -> return x
      Left _  -> fail $ "Invalid config value: " <> show text

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

-- | JSON encoding/decoding for communication with Python.
ednaAesonPythonOptions :: A.Options
ednaAesonPythonOptions = AC.aesonPrefix AC.snakeCase

----------------
-- Swagger
----------------

-- | Schema generation options which match JSON generation options.
schemaOptions :: SchemaOptions
schemaOptions = fromAesonOptions ednaAesonWebOptions

-- | Default implementation of 'ToSchema' via Generics.
gDeclareNamedSchema :: (Generic a, O.GToSchema (G.Rep a))
                    => Proxy a
                    -> Declare (O.Definitions O.Schema) O.NamedSchema
gDeclareNamedSchema = O.genericDeclareNamedSchema schemaOptions

-- | Default implementation of 'ToParamSchema' via Generics.
gToParamSchema :: (Generic a , O.GToParamSchema (G.Rep a))
               => Proxy a -> O.Schema
gToParamSchema = O.genericToParamSchema schemaOptions

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

oneOrError :: Monad m => Text -> [a] -> m a
oneOrError t = \case
  [x] -> pure x
  _ -> error t

localToUTC :: LocalTime -> UTCTime
localToUTC = localTimeToUTC utc

buildFromJSON :: ToJSON a => a -> Builder
buildFromJSON x = "" +| decodeUtf8 @Text (encode x) |+ ""

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

-- | Write a logging message unconditionally.
-- This function has 2 purposes:
--
-- 1. Ensures that logging goes to @stderr@.
-- 2. Prints the message and newline character together, as opposed to
-- @hPutStrLn@ which prints them as 2 different calls. When 2 messages are printed
-- concurrently using @hPutStrLn@, they may be printed on the same line followed
-- by 2 newline characters.
--
-- This function is not in 'Edna.Logging' to make it available for more modules
-- (avoiding cyclic dependencies).
logUnconditionally :: MonadIO m => Text -> m ()
logUnconditionally msg = hPutStr stderr (msg <> "\n")

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

instance Buildable (ForResponseLog (SqlId t)) where
  build = buildForResponse

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
