module Advent.Day11 where

import Data.List (sortOn)
import qualified Data.Map as M

import Advent.Util

import Debug.Trace

day :: Int
day = 11

type Coord      = (Int, Int)
type GridSN     = Int
type PowerLevel = Int
type Grid       = M.Map Coord PowerLevel

gridSize :: Int
gridSize = 300

powerLevel :: GridSN -> Coord -> PowerLevel
powerLevel sn (x, y) = hundreds - 5
  where rackID = x + 10
        initPL = (rackID * y + sn) * rackID
        hundreds = div (mod initPL 1000) 100

getCoords :: Int -> [Coord]
getCoords size = [(x, y) | x <- [1..size], y <- [1..size]]

buildGrid :: GridSN -> Grid
buildGrid sn = foldl setLevel M.empty coords
  where
    setLevel g coord = M.insert coord (powerLevel sn coord) g
    coords = getCoords gridSize

squareCoords :: Int -> Coord -> [Coord]
squareCoords size (x, y) = [(x', y') | x' <- [x .. x + size - 1], y' <- [y .. y + size - 1]]

buildSquares :: Int -> Grid -> Grid
buildSquares size g = foldl squareLevel M.empty coords
  where
    coords = getCoords (gridSize - size)
    squareLevel m coord =
      M.insert coord (sum $ map (g M.!) $ squareCoords size coord) m

part1 :: GridSN -> (Int, Int)
part1 = fst . last . sortOn snd . M.toList . buildSquares 3 . buildGrid

part2 :: GridSN -> (Int, (Coord, PowerLevel))
part2 sn = last $ sortOn (snd . snd) $ map findMaxSquare [11..20] --[1..300]
  where
    -- (size, ((x, y), pl))
    grid = buildGrid sn
    findMaxSquare size =
      (size , last $ sortOn snd $ M.toList $ buildSquares size grid)

main :: IO ()
main = do
  input <- read <$> readInput day
  print $ part1 input
  print $ part2 input  -- 236,175,11 (level=88)
