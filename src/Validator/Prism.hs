{-# LANGUAGE OverloadedStrings #-}

module Validator.Prism where

import           Control.Lens
import           Control.Lens.Prism
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Text
import qualified Data.Vector        as V
import           Validator.Types
import           Validator.Utility
import           Validator.Values

lng :: Prism' Value Lng
lng = prism' toJSON (\v -> toLng =<< v ^? _Number . _Double)

lat :: Prism' Value Lat
lat = prism' toJSON (\v -> toLat =<< v ^? _Number . _Double)

rightVersion :: Value -> Maybe ()
rightVersion o = o ^? _Number . _Integer . only mbglVersion

version :: JSON -> Maybe Value
version o = o ^. at "version"

name :: JSON -> Maybe Value
name o = o ^. at "name"

center :: JSON -> Maybe Value
center o = o ^. at "center"

zoom :: JSON -> Maybe Value
zoom o = o ^. at "zoom"

isDouble :: Value -> Maybe Double
isDouble o = o ^? _Number . _Double

toLngLat :: Value -> Maybe LngLat
toLngLat v = v ^? lnglat

lnglat :: Prism' Value LngLat
lnglat = prism' toJSON fromArray
  where
    fromArray v =
      LngLat <$>
        v ^? _Array . ix 0 . lng <*>
        v ^? _Array . ix 1 . lat

