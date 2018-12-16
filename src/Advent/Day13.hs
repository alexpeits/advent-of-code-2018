{-# LANGUAGE QuasiQuotes #-}
module Advent.Day13 where

import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State

import Text.RawString.QQ

import Advent.Util

day :: Int
day = 13

type Coord = (Int, Int)
type Grid  = M.Map Coord Char

data Cart = MkCart
  { cCoord     :: Coord
  , cDirection :: CartDirection
  , cNextTurn  :: CartTurn
  }
  deriving Show

data CartDirection = CDUp | CDDown | CDLeft | CDRight deriving Show
data CartTurn      = CTLeft | CTStraight | CTRight deriving Show

moveCoord :: Coord -> CartDirection -> Coord
moveCoord (x, y) d =
  case d of
    CDUp    -> (x, y - 1)
    CDDown  -> (x, y + 1)
    CDLeft  -> (x - 1, y)
    CDRight -> (x + 1, y)

stepCart :: Char -> Cart -> Cart
stepCart '|' (MkCart c d t) = MkCart (moveCoord c d) d t
stepCart '-' (MkCart c d t) = MkCart (moveCoord c d) d t
stepCart '/' (MkCart c d t) = MkCart (moveCoord c d') d' t
  where d' = case d of
          CDUp    -> CDRight
          CDDown  -> CDLeft
          CDLeft  -> CDDown
          CDRight -> CDUp
stepCart '\\' (MkCart c d t) = MkCart (moveCoord c d') d' t
  where d' = case d of
          CDUp    -> CDLeft
          CDDown  -> CDRight
          CDLeft  -> CDUp
          CDRight -> CDDown
stepCart '+' (MkCart c d t) = MkCart (moveCoord c d') d' t'
  where (d', t') = case (d, t) of
          (CDUp, CTLeft)     -> (CDLeft, CTStraight)
          (CDDown, CTLeft)   -> (CDRight, CTStraight)
          (CDLeft, CTLeft)   -> (CDDown, CTStraight)
          (CDRight, CTLeft)  -> (CDUp, CTStraight)
          (CDUp, CTRight)    -> (CDRight, CTLeft)
          (CDDown, CTRight)  -> (CDLeft, CTLeft)
          (CDLeft, CTRight)  -> (CDUp, CTLeft)
          (CDRight, CTRight) -> (CDDown, CTLeft)
          (_, CTStraight)    -> (d, CTRight)

buildGrid :: [String] -> Grid
buildGrid input = foldl addLine M.empty $ zip [0..] input
  where
    addLine m (y, l) = foldl (addChar y) m $ zip [0..] l
    addChar y m (x, c) = M.insert (x, y) c m

findCarts :: Grid -> [Cart]
findCarts =
  map mkCart
  . M.toList
  . M.filter (`elem` "^v<>")
  where mkCart (coord, c) = MkCart coord (f c) CTLeft
        f c = case c of
          '^' -> CDUp
          'v' -> CDDown
          '<' -> CDLeft
          '>' -> CDRight

initSystem :: [String] -> (Grid, [Cart])
initSystem input = (grid', carts)
  where grid = buildGrid input
        carts = findCarts grid
        grid' = M.map overwrite grid
        overwrite c
          | c `elem` "^v" = '|'
          | c `elem` "<>" = '-'
          | otherwise     = c

step :: Grid -> State [Cart] ()
step g = do
  carts <- get
  let carts' = map (\cart@MkCart{..} -> stepCart (g M.! cCoord) cart) carts
  put carts'

step' :: Grid -> State [Cart] ()
step' g = do
  coords <- gets (map cCoord)
  mapM_ stepOne' $ sort coords
  where
        stepOne' coord = do
          stepOne coord g
          carts' <- get
          let collisions = detectCollisions carts'
          put $ filter ((`notElem` collisions) . cCoord) carts'

stepOne :: Coord -> Grid -> State [Cart] ()
stepOne coord g = do
  carts <- get
  let carts' =
        map
          (\cart@MkCart{..} ->
             if cCoord == coord
             then stepCart (g M.! cCoord) cart
             else cart)
          carts
  put carts'

detectCollisions :: [Cart] -> [(Int, Int)]
detectCollisions = findSame . map cCoord

runToCrash :: Grid -> State [Cart] (Int, Int)
runToCrash g = do
  carts <- get
  case detectCollisions carts of
    [coord] -> return coord
    []      -> step g >> runToCrash g
    _       -> error "Is it possible?"

runUntilOne :: Grid -> State [Cart] (Int, Int)
runUntilOne g = do
  carts' <- get
  if length carts' == 1
    then return $ cCoord (head carts')
    else step' g >> runUntilOne g

part1 :: Grid -> [Cart] -> (Int, Int)
part1 grid = evalState (runToCrash grid)

part2 :: Grid -> [Cart] -> (Int, Int)
part2 grid = evalState (runUntilOne grid)

main :: IO ()
main = do
  rawInput <- readInputLines day
  let (grid, carts) = initSystem rawInput
  printResults day (part1 grid carts) (part2 grid carts)

testInput = [r|/>-<\
|   |
| /<+-\
| | | v
\>+</ |
  |   ^
  \<->/|]

test :: IO ()
test =
  let rawInput = lines testInput
      (grid, carts) = initSystem rawInput
  in printResults day "" (part2 grid carts)
