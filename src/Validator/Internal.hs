{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Validator.Internal where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.RWS.Lazy
import           Data.Aeson
import qualified Data.ByteString.Lazy   as BS
import           Data.HashMap.Lazy      (HashMap)
import qualified Data.HashMap.Lazy      as H hiding (HashMap)
import           Data.Maybe
import           Data.Scientific
import           Data.Sequence          (Seq, (<|), (><), (|>))
import qualified Data.Sequence          as Seq hiding (Seq, (<|), (><), (|>))
import qualified Data.Text              as T
import           Validator.Prism
import           Validator.Types
import           Validator.Utility
import           Validator.Values

-- Opens the file and tries to parse it as JSON.
readStyle :: (MonadIO m) => FilePath -> m Value
readStyle = liftIO . (return . fromMaybe Null . decode' <=< BS.readFile)

-- High-level validator for the style json.
-- Validates the overall structure of the document, and runs
-- the validator.
validateStyle :: Value -> Messages
validateStyle (Object o) = snd $ evalRWS validations o ()
validateStyle _          = Seq.singleton $ Err "Expected a JSON object"

-- parallelize?
validations :: Validator JSON ()
validations = validateRoot

validateRoot :: Validator JSON ()
validateRoot = checkVersion
            >> checkName
            >> checkCenter
            >> checkZoom

checkVersion :: Validator JSON ()
checkVersion = asks version
           >>= maybe (required "version") compareToSpec
  where
    compareToSpec = maybe wrongVersion return . rightVersion
    wrongVersion = tell $ err "'version' must be 8"

checkName :: Validator JSON ()
checkName = asks name
        >>= isString
  where isString (Just (String t))
          | T.null $ T.strip t = tell $ warn "'name' is an empty string"
          | otherwise          = return ()
        isString (Just _)          = tell $ err "'name' must be a string"
        isString _                 = return ()

checkCenter :: Validator JSON ()
checkCenter = asks center >>= maybe nothing checkCoords
  where checkCoords = maybe notValidLngLat (const nothing) . toLngLat
        notValidLngLat = tell $ err "'center' is not a valid lnglat pair"

checkZoom :: Validator JSON ()
checkZoom = asks zoom >>= maybe nothing checkZoomLevel
  where checkZoomLevel = maybe notValidDouble (const nothing) . isDouble
        notValidDouble = tell $ err "'zoom' is not a valid zoom level"
