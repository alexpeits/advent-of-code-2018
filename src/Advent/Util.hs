{-# LANGUAGE TypeFamilies #-}
module Advent.Util where

import System.FilePath ((</>), (<.>))
import Text.Printf (printf)

import qualified Data.Map as M
import qualified Data.Set as S

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

-- insertAppend :: Ord a => a -> b -> M.Map a [b] -> M.Map a [b]
-- insertAppend k v m =
--   M.insert k (v:v') m
--   where v' = M.findWithDefault [] k m

type family Elem container where
  Elem [a]       = a
  Elem (S.Set a) = a

insertWithDefault :: Ord a => (Elem b -> b -> b) -> b -> a -> Elem b -> M.Map a b -> M.Map a b
insertWithDefault f d k v m =
  M.insert k (f v v') m
  where v' = M.findWithDefault d k m
