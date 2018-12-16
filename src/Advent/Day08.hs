module Advent.Day08 where

import qualified Data.Tree as T

import Advent.Util

import Debug.Trace

day :: Int
day = 8

type Metadata = [Int]

newtype Node
  = MkNode { getMeta :: Metadata }
  deriving Show

type License = T.Tree Node

parseTree :: [Int] -> T.Tree Node
parseTree input@(_ : nMetadata : rest) = fst $ go input
  where
    go (0 : nm : r)
      = (T.Node (MkNode metadata) [], r')
      where (metadata, r') = splitAt nm r
    go (nc : nm : i)
      = (T.Node (MkNode metadata) (reverse ch), r')
      where (ch, r) = foldl
                        (\(ch, r) _ -> let (c, r') = go r in (c:ch, r'))
                        ([], i)
                        (replicate nc ())
            (metadata, r') = splitAt nm r

part1 :: License -> Int
part1 = T.foldTree (\MkNode{..} xs -> sum getMeta + sum xs)

part2 :: License -> Int
part2 = nodeValue
  where
    nodeValue (T.Node MkNode{..} []) = sum getMeta
    nodeValue (T.Node MkNode{..} ch)
      = sum
      $ map (maybe 0 nodeValue . (ch !?) . pred) getMeta

main :: IO ()
main = do
  rawInput <- readInputWords day
  let input :: [Int]
      input = map read rawInput
      tree = parseTree input
  printResults day (part1 tree) (part2 tree)
