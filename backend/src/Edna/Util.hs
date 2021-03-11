module Edna.Util
  ( NetworkAddress (..)
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
-- Aeson
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
