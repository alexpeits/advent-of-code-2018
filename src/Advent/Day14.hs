module Advent.Day14 where

import Control.Monad.State
import qualified Data.IntMap.Strict as M

import Advent.Util

day :: Int
day = 14

type Recipes = M.IntMap Int

data St = St
  { stRecipes   :: Recipes
  , stElves     :: [Int]
  , stLen       :: Int
  , stSubstr    :: [Int]
  }
  deriving Show

intToList :: Int -> [Int]
intToList = map (read . return) . show

step :: State St ()
step = do
  St{..} <- get
  let elfScores = map (stRecipes M.!) stElves
      newDigits = intToList $ sum elfScores
      stRecipes' = foldl (\m (k, v) -> M.insert k v m) stRecipes $ zip [stLen..] newDigits
      stLen' = stLen + length newDigits
      stElves' = map ((`mod` stLen') . (+ 1) . uncurry (+)) $ zip stElves elfScores
      stSubstr' = drop (length newDigits) $ stSubstr ++ newDigits
  put $ St stRecipes' stElves' stLen' stSubstr'

runLimit :: Int -> Int -> State St Recipes
runLimit input limit = do
  step
  St{..} <- get
  if stLen >= input + limit
    then return stRecipes
    else runLimit input limit

runSubstr :: [Int] -> State St Recipes
runSubstr target = do
  step
  St{..} <- get
  if stSubstr == target
    then return stRecipes
    else runSubstr target

part1 :: Int -> Int -> String
part1 input limit = mconcat $ map (show . snd) $ take limit $ drop input $ M.toList recipes
  where recipes = evalState (runLimit input limit) initSt
        initSt = St (M.fromList [(0, 3), (1, 7)]) [0, 1] 2 (replicate substrLen 0)
        substrLen = length $ show input

part2 :: Int -> Int
part2 input = (subtract substrLen) $ length $ M.toList recipes
  where recipes = evalState (runSubstr target) initSt
        target = intToList input
        substrLen = length target
        initSt = St (M.fromList [(0, 3), (1, 7)]) [0, 1] 2 (replicate substrLen 0)

main :: IO ()
main = do
  input <- read <$> readInput day
  -- printResults day (part1 input 10) (part2 input)
  printResults day "" (part2 input)
  -- printResults day "" (part2 51589)
