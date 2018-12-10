{-# LANGUAGE RecordWildCards #-}
module Advent.Day06 where

import qualified Data.Map as M
import Control.Arrow ((***), (&&&))

import Text.Parsec
import Text.Parsec.String (Parser)

import Advent.Util

day :: Int
day = 6

type Grid = M.Map Coord

manhattan :: Coord -> Coord -> Int
manhattan (MkCoord x1 y1) (MkCoord x2 y2) =
  abs (x1 - x2) + abs (y1 - y2)

data Coord = MkCoord
  { coordX :: Int
  , coordY :: Int
  }
  deriving (Eq, Show)

instance Ord Coord where
  (MkCoord x1 y1) <= (MkCoord x2 y2) =
    x1 <= x2 && y1 <= y2

parseCoord :: Parser Coord
parseCoord = do
  coordX <- parseNum
  char ',' >> spaces
  coordY <- parseNum
  return MkCoord{..}

inBounds :: [Coord] -> (Coord -> Bool)
inBounds cs = \(MkCoord x y) ->
  x >= minX && x <= maxX && y >= minY && y <= maxY
  where (maxX, maxY) = (maximum *** maximum) xys
        (minX, minY) = (minimum *** minimum) xys
        xys = unzip $ map (coordX &&& coordY) cs

main :: IO ()
main = do
  rawInput <- readInputLines day
  let (Right input) = mapM (parse parseCoord "") rawInput
  printResults day "" ""
