module Advent.Util
  ( readInput
  , readInputWords
  , readInputLines
  , printResults
  , parseNum
  , counter
  ) where

import System.FilePath ((</>), (<.>))
import Text.Printf (printf)

import qualified Data.Map as M

import Text.Parsec (many1, digit)
import Text.Parsec.String (Parser)


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

parseNum :: (Read a, Num a) => Parser a
parseNum = read <$> many1 digit

counter :: Ord a => [a] -> M.Map a Int
counter xs = M.fromListWith (+) $ zip xs (repeat 1)
