module Main where

import           Control.Monad
import           Data.Maybe
import           System.Environment
import           System.Exit
import           Validator

main :: IO ()
main = do
  fp <- listToMaybe <$> getArgs
  unless (isJust fp) (putStrLn "No file specified" >> exitFailure)
  print . validateStyle =<< readStyle (fromJust fp)

