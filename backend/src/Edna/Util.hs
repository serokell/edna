module Edna.Util
   ( NetworkAddress (..)
   ) where

import Universum

import Data.Aeson
  (FromJSON(..), ToJSON(..), Value(..), withText)
import Text.Read (Read (..), read)
import Fmt (Buildable (..), (+|), (|+))
import Data.Text.Lazy.Builder (toLazyText)
import qualified Text.ParserCombinators.ReadP as ReadP
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
  show = toString . pretty

instance FromJSON NetworkAddress where
  parseJSON = withText "NetworkAddress" $ pure . read . toString

instance ToJSON NetworkAddress where
  toJSON = String . pretty

prettyL :: Buildable a => a -> LText
prettyL = toLazyText . build

pretty :: Buildable a => a -> Text
pretty = toStrict . prettyL
