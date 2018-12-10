module Advent.Day05 where

import qualified Data.Char as C
import Control.Monad.State.Strict

import Advent.Util
import qualified Advent.ListZipper as Z

day :: Int
day = 5

type Polymer = Z.ListZipper Char

opposite :: Char -> Char -> Bool
opposite x y =
  not (all C.isLower both)
  && not (all C.isUpper both)
  && (C.toLower x == C.toLower y)
  where both = [x, y]

step :: State Polymer ()
step = do
  p@(Z.MkListZipper left focus right) <- get
  let t [] = []
      t [_] = []
      t (_:xs) = xs
  case right of
    [] -> pure ()
    (x:xs) ->
      if not (opposite focus x)
      then put $ Z.forward p
      else
        case left of
          [] -> put $ Z.MkListZipper [] (head xs) (t xs)
          _  -> put $ Z.MkListZipper (t left) (head left) xs
  pure ()

runPolymer :: State Polymer ()
runPolymer = do
  step
  Z.MkListZipper left focus right <- get
  case right of
    [] -> pure ()
    _  -> runPolymer

part1 :: String -> Int
part1 =
  length
  . Z.toList
  . execState runPolymer
  . Z.fromList

part2 :: String -> Int
part2 =
  minimum
  . map ( length
        . Z.toList
        . execState runPolymer
        . Z.fromList
        )
  . zipWith
      (\chars s -> filter (`notElem` chars) s)
      (zipWith (\x y -> [x, y]) ['a'..'z'] ['A'..'Z'])
  . repeat

main :: IO ()
main = do
  input <- head . lines <$> readInput day
  printResults day (part1 input) (part2 input)
