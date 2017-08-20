module Validator.Values where

mbglVersion :: Integer
mbglVersion = 8

minLng, maxLng, minLat, maxLat :: Double
minLng = -180
maxLng = 180
minLat = -90
maxLat = 90

minBearing, maxBearing :: Double
minBearing = -360
maxBearing = 360

minPitch, maxPitch :: Double
minPitch = 0
maxPitch = 60

minZoom, maxZoom :: Integer
minZoom = 0
maxZoom = 20
