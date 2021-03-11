module Edna.Util
  ( NetworkAddress (..)
  , ConnString (..)
  , DatabaseInitOption (..)
  , ednaAesonWebOptions
  , ednaAesonConfigOptions
  , schemaOptions
  , gDeclareNamedSchema
  , gToParamSchema
  ) where

import Universum

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), withText)
import qualified Data.Aeson as A
import qualified Data.Aeson.Casing as AC
import qualified Data.Swagger as S
import Data.Swagger.Declare (Declare)
import Data.Swagger.Internal (ParamSchema)
import Data.Swagger.Internal.ParamSchema (GToParamSchema, genericToParamSchema)
import qualified Data.Swagger.Internal.Schema as S
import Data.Swagger.Internal.TypeShape (GenericHasSimpleShape, GenericShape)
import Data.Swagger.SchemaOptions (SchemaOptions, fromAesonOptions)
import Fmt (Buildable(..), pretty, (+|), (|+))
import qualified GHC.Generics as G
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
