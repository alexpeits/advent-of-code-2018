module Advent.Day07 where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Parsec
import Text.Parsec.String (Parser)

import Advent.Util

import Debug.Trace

day :: Int
day = 7

type Step = Char

data Instruction = Order Step Step
  deriving Show

type Graph = M.Map Step (S.Set Step)
type SQueue = [Step]

enqueue :: SQueue -> Step -> SQueue
enqueue = flip L.insert

dequeue :: SQueue -> Maybe (Step, SQueue)
dequeue []     = Nothing
dequeue (x:xs) = Just (x, xs)

parseInstruction :: Parser Instruction
parseInstruction = do
  string "Step" >> spaces
  start <- satisfy C.isUpper
  spaces >> string "must be finished before step" >> spaces
  finish <- satisfy C.isUpper
  spaces >> string "can begin."
  pure $ Order start finish

buildGraph :: [Instruction] -> Graph
buildGraph =
  foldl (\m (Order from to) -> insertWithDefault S.insert S.empty from to m) M.empty

part1 :: [Instruction] -> [Step]
part1 instrs = reverse $ go firsts []
  where gr = buildGraph instrs
        keys = S.fromList $ M.keys gr
        allValues = S.unions $ M.elems gr
        firsts = foldl enqueue [] $ S.toList $ S.difference keys allValues
        predicates s =
          S.fromList
          $ M.keys
          $ M.filter (S.member s) gr
        isReady res s =
          let p = predicates s
          in S.null $ S.difference p (S.fromList res)
        nextReady :: [Step] -> SQueue -> (Step, SQueue)
        nextReady res q =
          let (Just i) = L.findIndex (isReady res) q
              (left, x:right) = L.splitAt i q
          in (x, left ++ right)
        go [] r = r
        go q r =
          let (x, q') = nextReady r q
              newR = x:r
              xs' = S.toList (M.findWithDefault S.empty x gr)
              newQ = L.nub $ foldl enqueue q' xs'
          in go newQ newR

main :: IO ()
main = do
  rawInput <- readInputLines day
  let (Right input) = mapM (parse parseInstruction "") rawInput
  printResults day (part1 input) ""

test :: IO [Instruction]
test = do
  rawInput <- readInputLines day
  let (Right input) = mapM (parse parseInstruction "") rawInput
  return input
