{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Advent.Day12 where

import qualified Data.Map as M
import Control.Monad.State

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)

import Advent.Util

import Text.RawString.QQ

day :: Int
day = 12

type Pot = Bool
type Rules = M.Map [Pot] Pot

parsePot :: Parser Pot
parsePot = (== '#') <$> (char '#' <|> char '.')

-- Also pad the initial state to make the rest easier
parseInitState :: Parser [Pot]
parseInitState = do
  string "initial state:" >> spaces
  pots <- many1 parsePot
  return $ replicate 5 False ++ pots ++ replicate 5 False

parseRule :: Parser ([Pot], Pot)
parseRule = do
  from <- many1 parsePot
  spaces >> string "=>" >> spaces
  to <- parsePot
  return (from, to)

makeRules :: [([Pot], Pot)] -> Rules
makeRules = foldl (\m (from, to) -> M.insert from to m) M.empty

step :: Rules -> State [Pot] ()
step rules = do
  pots <- get
  let chunks = map (\i -> take 5 (drop i pots)) [0 .. length pots - 5]
      -- repl chunk = M.findWithDefault (chunk !! 2) chunk rules
      repl chunk = rules M.! chunk
      pots' = map repl chunks
  put $ False : False : pots' ++ replicate 2 False
  return ()

main :: IO ()
main = do
  rawInput <- readInputLines day
  let (Right initState) = parse parseInitState "" (head rawInput)
      (Right rules)  = makeRules <$> mapM (parse parseRule "") (drop 2 rawInput)
  showPots $ execState (step rules) initState

showPots :: [Pot] -> IO ()
showPots = putStrLn . map (\x -> if x then '#' else '.')

testInput :: String
testInput = [r|initial state: #..#.#..##......###...###

...## => #
..#.. => #
.#... => #
.#.#. => #
.#.## => #
.##.. => #
.#### => #
#.#.# => #
#.### => #
##.#. => #
##.## => #
###.. => #
###.# => #
####. => #|]

test :: IO ()
test = do
  let rawInput = lines testInput
      (Right initState) = parse parseInitState "" (head rawInput)
      (Right rules)  = makeRules <$> mapM (parse parseRule "") (drop 2 rawInput)
      -- chunks = map (\i -> take 5 (drop i initState)) [0 .. length initState - 5]
      -- repl chunk = M.findWithDefault (chunk !! 2) chunk rules
      -- pots' = map repl chunks
  -- mapM_ showPots $ chunks
  showPots $ execState (step rules) initState
