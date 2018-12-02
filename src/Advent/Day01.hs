module Advent.Day01 where

import qualified Data.Set as Set
import Advent.Util

day :: Int
day = 1

part1 :: [Int] -> Int
part1 = sum

part2 :: [Int] -> Int
part2 = go Set.empty . scanl (+) 0 . cycle
  where go :: Set.Set Int -> [Int] -> Int
        go s (x:xs) =
          if x `Set.member` s
          then x
          else go (Set.insert x s) xs

main :: IO ()
main = do
  rawInput <- readInputLines day
  let input :: [Int]
      input = map (read . filter (/= '+')) rawInput
  printResults day (part1 input) (part2 input)
