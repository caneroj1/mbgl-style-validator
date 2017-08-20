{-# LANGUAGE OverloadedStrings #-}

module Validator.Prism where

import           Control.Lens
import           Control.Lens.Prism
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.Text
import qualified Data.Vector        as V
import           Validator.Types
import           Validator.Utility
import           Validator.Values

lng :: Prism' Value Lng
lng = prism' toJSON (parseMaybe parseJSON)

lat :: Prism' Value Lat
lat = prism' toJSON (parseMaybe parseJSON)

lnglat :: Prism' Value LngLat
lnglat = prism' toJSON (parseMaybe parseJSON)

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

bearing :: JSON -> Maybe Value
bearing o = o ^. at "bearing"

pitch :: JSON -> Maybe Value
pitch o = o ^. at "pitch"

isDouble :: Value -> Maybe Double
isDouble o = o ^? _Number . _Double

toLngLat :: Value -> Maybe LngLat
toLngLat v = v ^? lnglat
