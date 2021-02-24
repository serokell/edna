module Edna.Util
  ( NetworkAddress (..)
  , ednaAesonWebOptions
  , ednaAesonConfigOptions
  ) where

import Universum

import qualified Data.Aeson.Casing as AC
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), withText)
import qualified Data.Aeson as A
import Fmt (Buildable(..), pretty, (+|), (|+))
import qualified Text.ParserCombinators.ReadP as ReadP
import Text.Read (Read(..), read)
import qualified Text.Show

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

ednaAesonConfigOptions :: A.Options
ednaAesonConfigOptions = AC.aesonPrefix AC.trainCase

ednaAesonWebOptions :: A.Options
ednaAesonWebOptions = AC.aesonPrefix AC.camelCase
