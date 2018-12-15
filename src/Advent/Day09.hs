{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Advent.Day09 where

import Data.Function ((&))

import Control.Lens
import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)

import Advent.Util
import qualified Advent.ListZipper as Z

import Debug.Trace

day :: Int
day = 9

type Marble = Int

data Rules = MkRules
  { _rPlayers   :: Int
  , _rMarbles :: Marble
  }
  deriving Show

makeLenses ''Rules

type Player = [Marble]
type Circle = Z.ListZipperC Marble

data Game = MkGame
  { gPlayers :: [Player]
  , gCircle :: Circle
  }
  deriving Show

initGame :: Int -> Game
initGame np = MkGame (replicate np []) (Z.fromList [0])

parseRules :: Parser Rules
parseRules = do
  _rPlayers <- parseNum
  string " players; last marble is worth "
  _rMarbles <- parseNum
  return MkRules{..}

part1 :: Rules -> Int
part1 MkRules{..} = maximum $ map sum $ gPlayers runGame
  where
    runGame = foldl play (initGame _rPlayers) turns
    turns = zip [1.._rMarbles] (cycle [0.._rPlayers - 1])

    play :: Game -> (Marble, Int) -> Game
    play g (m, i)
      | m `mod` 23 == 0 = play23 g m i
      | otherwise       = playReg g m i

    playReg MkGame{..} m _ =
      let gCircle' = gCircle & Z.forward' & Z.insertRight m & Z.forward'
      in MkGame gPlayers gCircle'
    play23 MkGame{..} m i =
      let (m', gCircle') = gCircle & Z.backwardN' 6 & Z.removeLeft'
          gPlayers' = gPlayers & ix i %~ (\ms -> m':m:ms)
      in MkGame gPlayers' gCircle'

main :: IO ()
main = do
  rawInput <- readInput day
  let (Right input1) = parse parseRules "" rawInput
      input2         = input1 & rMarbles %~ (* 100)
  -- Compile with -O2 for part 2 plz
  printResults day (part1 input1) (part1 input2)
