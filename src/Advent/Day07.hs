module Advent.Day07 where

import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Maybe (catMaybes)

import Control.Monad.State

import Text.Parsec hiding (State)
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

data Worker = MkWorker
  { elapsed :: Int
  , step    :: Step
  , total   :: Int
  } deriving Show

data St = St
  { workers    :: [Worker]
  , maxWorkers :: Int
  , queue      :: SQueue
  , clock      :: Int
  , steps      :: [Step]
  } deriving Show

startWorker :: Step -> Int -> Worker
startWorker = MkWorker 0

run :: [Instruction] -> Int
run instrs = undefined
  where
    -- dependency graph
    g = buildGraph instrs
    -- reverse dependency graph
    rg = foldl (\m k -> M.insert k (preds' k) m) M.empty (M.keys g)

    -- what keys does `k` depend on?
    preds' k = S.fromList $ M.keys $ M.filter (S.member k) g
    -- same but with reverse dependencies for speed
    preds k = M.findWithDefault S.empty k rg

    -- get first items to enqueue
    keys = S.fromList $ M.keys g
    allValues = S.unions $ M.elems g
    firsts = foldl enqueue [] $ S.toList $ S.difference keys allValues

    -- is `k` ready based on current done items?
    -- `k` has to _not_ be an element of currently processing items
    isReady
      :: [Step]  -- done
      -> [Step]  -- currently processing
      -> Char    -- key
      -> Bool
    isReady done pr k =
      k `notElem` pr
      && S.null (S.difference (preds k) (S.fromList done))
    -- get next item ready for processing and return new state
    nextReady
      :: [Step]  -- done
      -> [Step]  -- currently processing
      -> SQueue  -- queue
      -> (Maybe Step, SQueue)  -- a step and the new queue
    nextReady done pr q =
      case mi of
        Nothing -> (Nothing, q)
        (Just i) ->
          let (left, x:right) = L.splitAt i q
          in (Just x, left ++ right)
      where mi = L.findIndex (isReady done pr) q

initSt :: Int -> [Instruction] -> St
initSt maxWorkers instrs = St{..}
  where workers = []
        -- queue = foldl enqueue [] $ S.toList $ S.difference keys allValues
        clock = 0

stepProcess :: Graph -> Graph -> State St ()
stepProcess g rg = do
  s@(St w mw q c ss) <- get
  let wip = length w
      wrem = mw - wip
      w' =
        filter ((<) <$> elapsed <*> total)
        $ map (MkWorker <$> (+1) . elapsed <*> step <*> total) w
  return ()

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
