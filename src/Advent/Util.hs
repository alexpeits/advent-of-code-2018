module Advent.Util
  ( readInput
  , readInputWords
  , readInputLines
  , printResults
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

printResults :: (Show a, Show b) => Int -> a -> b -> IO ()
printResults day part1 part2 = do
  putStrLn $ "Day " <> show day <> " results:"
  putStrLn $ "Part 1: " <> show part1
  putStrLn $ "Part 2: " <> show part2

