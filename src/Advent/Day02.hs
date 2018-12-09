{-# LANGUAGE TupleSections #-}
module Advent.Day02 where

import Data.List (nub, tails)
import qualified Data.Map as M

import Advent.Util

day :: Int
day = 2

part1 :: [String] -> Int
part1 =
  product
  . M.elems
  . counter
  . concatMap ( nub
              . filter (`elem` [2, 3])
              . M.elems
              . counter
              )

combinations :: [a] -> [(a, a)]
combinations = concatMap comb . init . tails
  where comb (x:xs) = map (x,) xs

differing :: Int -> String -> String -> Bool
differing n x y = count > n
  where
    count = length $ filter (uncurry (/=)) $ zip x y

part2 :: [String] -> String
part2 =
  map fst
  . filter (uncurry (==))
  . uncurry zip
  . head
  . dropWhile (uncurry (differing 1))
  . combinations

main :: IO ()
main = do
  input <- readInputLines day
  printResults day (part1 input) (part2 input)
