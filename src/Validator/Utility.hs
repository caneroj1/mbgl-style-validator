{-# LANGUAGE OverloadedStrings #-}

module Validator.Utility where

import           Control.Lens
import           Control.Lens.Prism
import           Control.Monad.RWS.Lazy
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Scientific
import qualified Data.Sequence          as Seq
import           Data.Text              (Text)
import qualified Data.Text              as T hiding (Text)
import           Validator.Types

inBounds :: (Bounded a, Ord a) => a -> Bool
inBounds a = minBound <= a && a <= maxBound

toLng :: Double -> Maybe Lng
toLng d = let lng = Lng d in if inBounds lng then Just lng else Nothing

toLat :: Double -> Maybe Lat
toLat d = let lat = Lat d in if inBounds lat then Just lat else Nothing

err :: Text -> Messages
err = Seq.singleton . Err

warn :: Text -> Messages
warn = Seq.singleton . Warn

nothing :: Validator a ()
nothing = return ()

required :: Text -> Validator a ()
required t =
  let msg = "'" `T.append` t `T.append` "' is required"
    in tell $ err msg
