-- | Implementation of 4PL (four parameter logistic) analysis.

module Edna.Analysis.FourPL
  ( Params4PL (..)
  , analyse4PL
  ) where

import Universum

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Swagger (ToSchema(..))
import Fmt (Buildable(..), tupleF)
import Servant.Util.Combinators.Logging (ForResponseLog, buildForResponse)

-- | Parameters of 4PL function, analysis outcome of this analysis method.
-- The function is defined as follows:
-- @f(x) = d + (a - d) / (1 + (x / c)^b)@
--
-- 4 fields of this type are @a@, @b@, @c@ and @d@ parameters respectively.
data Params4PL = Params4PL
  { p4plA :: Double
  , p4plB :: Double
  , p4plC :: Double
  , p4plD :: Double
  } deriving stock (Generic, Show, Eq)

type Params4PLTuple = (Double, Double, Double, Double)

toTuple :: Params4PL -> Params4PLTuple
toTuple Params4PL {..} = (p4plA, p4plB, p4plC, p4plD)

instance Buildable Params4PL where
  build = tupleF . toTuple

instance Buildable (ForResponseLog Params4PL) where
  build = buildForResponse

-- Serializing as a tuple for brevity.
instance ToJSON Params4PL where
  toJSON = toJSON . toTuple
  toEncoding = toEncoding . toTuple

instance FromJSON Params4PL where
  parseJSON = fmap constr . parseJSON
    where
      constr (a, b, c, d) = Params4PL a b c d

instance ToSchema Params4PL where
  declareNamedSchema Proxy =
    declareNamedSchema @Params4PLTuple Proxy

-- | This function performs actual analysis and finds the best parameters.
--
-- TODO [EDNA-71] Implement!
analyse4PL :: [(Double, Double)] -> IO Params4PL
analyse4PL _ = pure (Params4PL 36404 1.14 33 -2552)
