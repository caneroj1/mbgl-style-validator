module Validator.Types where

import           Control.Monad.RWS.Lazy
import           Data.Aeson
import           Data.HashMap.Lazy      (HashMap)
import           Data.Sequence          (Seq)
import           Data.Text
import           Validator.Values

-- A style message represents an output message that will be displayed.
-- Err constructs an error message, Warn constructs warnings, and
-- Fix constructs messages on how to improve the style, if any.
data StyleMessage = Err Text
                  | Warn Text
                  | Fix Text

instance Show StyleMessage where
  show (Err t)  = "[Error]: " ++ show t
  show (Warn t) = "[Warning]: " ++ show t
  show (Fix t)  = "[Fix]: " ++ show t

-- Output sequence of messages.
type Messages = Seq StyleMessage

-- Type alias for the validation transformer stack.
type Validator r a = RWS r Messages () a

-- Type alias for underlying json object in an Aeson Object.
type JSON = HashMap Text Value

newtype Lng = Lng Double
  deriving (Show, Eq, Ord)

instance ToJSON Lng where
  toJSON (Lng l) = toJSON l

instance Bounded Lng where
  minBound = Lng minLng
  maxBound = Lng maxLng

newtype Lat = Lat Double
  deriving (Show, Eq, Ord)

instance ToJSON Lat where
  toJSON (Lat l) = toJSON l

instance Bounded Lat where
  minBound = Lat minLat
  maxBound = Lat maxLat

data LngLat = LngLat Lng Lat

instance ToJSON LngLat where
  toJSON (LngLat (Lng lng) (Lat lat)) = toJSON [lng, lat]
