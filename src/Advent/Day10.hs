module Advent.Day10 where

import Control.Monad.State
import Control.Monad (forM_)
import qualified Data.Set as S

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)

import Advent.Util

day :: Int
day = 10

type Coord    = (Int, Int)
type Velocity = (Int, Int)

data Point = MkPoint
  { pPosition :: Coord
  , pVelocity :: Velocity
  }
  deriving Show

type Sky = [Point]
-- ((min x, max x), (min y, max y))
type Bounds = ((Int, Int), (Int, Int))

parsePoint :: Parser Point
parsePoint = do
  string "position="
  pPosition <- parsePair
  spaces >> string "velocity="
  pVelocity <- parsePair
  return MkPoint{..}
  where parsePair = do
          char '<' >> spaces
          x <- parseNum
          char ',' >> spaces
          y <- parseNum
          char '>'
          return (x, y)

stepPoint :: Point -> Point
stepPoint (MkPoint (px, py) v@(vx, vy))
  = MkPoint (px + vx, py + vy) v

stepSky :: State (Sky, Int) Bounds
stepSky = do
  (sky, elapsed) <- get
  let (sky', bounds) = foldl go ([], ((maxBound, minBound), (maxBound, minBound))) sky
  put (sky', elapsed + 1)
  return bounds
  where
    go (ps, ((minX, maxX), (minY, maxY))) p
      = let p'@(MkPoint (px, py) v) = stepPoint p
        in (p':ps, ((min minX px, max maxX px), (min minY py, max maxY py)))

showSky :: Sky -> Bounds -> IO ()
showSky sky ((minX, maxX), (minY, maxY)) = forM_ ys printLine
  where
    printLine y = forM_ xs (printPoint y) >> putStrLn ""
    printPoint y x = putStr $ if (x, y) `S.member` points then "#" else "."
    points = S.fromList $ map pPosition sky
    xs = [minX..maxX]
    ys = [minY..maxY]

run :: Bounds -> State (Sky, Int) Bounds
run b@((minX, maxX), (minY, maxY)) = do
  (sky, elapsed) <- get
  b'@((minX', maxX'), (minY', maxY')) <- stepSky
  if (maxX' - minX') > (maxX - minX) && (maxY' - minY') > (maxY - minY)
    then put (sky, elapsed) >> return b
    else run b'

part1and2 :: Sky -> IO ()
part1and2 sky =
  let (bounds', (sky', elapsed))
        = runState (run ((0, 1000000000), (0, 10000000000))) (sky, 0)
  in showSky sky' bounds' >> print elapsed

main :: IO ()
main = do
  rawInput <- readInputLines day
  let (Right input) = mapM (parse parsePoint "") rawInput
  part1and2 input
