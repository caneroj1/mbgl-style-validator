{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Validator.Types where

import           Control.Lens           hiding (Zoom)
import           Control.Lens.Prism
import           Control.Monad
import           Control.Monad.RWS.Lazy
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.HashMap.Lazy      (HashMap)
import           Data.Scientific
import           Data.Sequence          (Seq)
import           Data.Text
import           Validator.Values

asBoundedDouble :: (Bounded a, RealFloat a) => Value -> Maybe a
asBoundedDouble v = either (const Nothing) Just . toBoundedRealFloat =<< v ^? _Number

asBoundedInt :: (Bounded a, Integral a) => Value -> Maybe a
asBoundedInt v = toBoundedInteger =<< v ^? _Number

maybeToParser :: Maybe a -> Parser a
maybeToParser = maybe mzero return

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
  deriving (Show, Eq, Ord, Fractional, Num, Real, RealFrac, Floating, RealFloat)

instance ToJSON Lng where
  toJSON (Lng l) = toJSON l

instance Bounded Lng where
  minBound = Lng minLng
  maxBound = Lng maxLng

instance FromJSON Lng where
  parseJSON = maybeToParser . asBoundedDouble

newtype Lat = Lat Double
  deriving (Show, Eq, Ord, Fractional, Num, Real, RealFrac, Floating, RealFloat)

instance ToJSON Lat where
  toJSON (Lat l) = toJSON l

instance Bounded Lat where
  minBound = Lat minLat
  maxBound = Lat maxLat

instance FromJSON Lat where
  parseJSON = maybeToParser . asBoundedDouble

data LngLat = LngLat Lng Lat

instance ToJSON LngLat where
  toJSON (LngLat (Lng lng) (Lat lat)) = toJSON [lng, lat]

instance FromJSON LngLat where
  parseJSON v =
    LngLat <$>
      maybeToParser (asBoundedDouble =<< (v ^? _Array . ix 0)) <*>
      maybeToParser (asBoundedDouble =<< (v ^? _Array . ix 1))

newtype Bearing = Bearing Double
  deriving (Show, Eq, Ord, Fractional, Num, Real, RealFrac, Floating, RealFloat)

instance ToJSON Bearing where
  toJSON (Bearing b) = toJSON b

instance Bounded Bearing where
  minBound = Bearing minBearing
  maxBound = Bearing maxBearing

instance FromJSON Bearing where
  parseJSON = maybeToParser . asBoundedDouble

newtype Pitch = Pitch Double
  deriving (Show, Eq, Ord, Fractional, Num, Real, RealFrac, Floating, RealFloat)

instance ToJSON Pitch where
  toJSON (Pitch b) = toJSON b

instance Bounded Pitch where
  minBound = Pitch minPitch
  maxBound = Pitch maxPitch

instance FromJSON Pitch where
  parseJSON = maybeToParser . asBoundedDouble

newtype Zoom = Zoom Integer
  deriving (Show, Eq, Ord, Integral, Num, Real, Enum)

instance ToJSON Zoom where
  toJSON (Zoom z) = toJSON z

instance Bounded Zoom where
  minBound = Zoom minZoom
  maxBound = Zoom maxZoom

instance FromJSON Zoom where
  parseJSON = maybeToParser . asBoundedInt
