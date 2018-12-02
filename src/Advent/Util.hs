module Advent.Util
  ( readInput
  , readInputWords
  , readInputLines
  ) where

import System.FilePath ((</>), (<.>))
import Text.Printf (printf)

getInputPath :: Int -> FilePath
getInputPath day = "inputs" </> printf "day%02d" day <.> "txt"

readInput :: Int -> IO String
readInput = readFile . getInputPath

readInputWords :: Int -> IO [String]
readInputWords = fmap words . readInput

readInputLines :: Int -> IO [String]
readInputLines = fmap lines . readInput

