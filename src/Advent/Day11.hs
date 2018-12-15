module Advent.Day11 where

import Advent.Util

day :: Int
day = 11

main :: IO ()
main = do
  input <- read <$> readInput day :: IO Int
  print input
