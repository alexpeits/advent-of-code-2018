{-# LANGUAGE TypeFamilies #-}
module Advent.Util where

import System.FilePath ((</>), (<.>))
import Text.Printf (printf)

import Control.Monad ((<=<))
import Data.List (group, sort, nub)
import qualified Data.Map as M
import qualified Data.Set as S

import Text.Parsec (many1, digit, char, (<|>))
import Text.Parsec.String (Parser)


getInputPath :: Int -> FilePath
getInputPath day = "inputs" </> printf "day%02d" day <.> "txt"

readInput :: Int -> IO String
readInput = readFile . getInputPath

readInputWords :: Int -> IO [String]
readInputWords = fmap words . readInput

readInputLines :: Int -> IO [String]
readInputLines = fmap lines . readInput

printResults :: (Show a, Show b) => Int -> a -> b -> IO ()
printResults day part1 part2 = do
  putStrLn $ "Day " <> show day <> " results:"
  putStrLn $ "Part 1: " <> show part1
  putStrLn $ "Part 2: " <> show part2

parseNum :: (Read a, Num a) => Parser a
parseNum = (char '-' >> (negate <$> parseNum')) <|> parseNum'
  where parseNum' = read <$> many1 digit

counter :: Ord a => [a] -> M.Map a Int
counter xs = M.fromListWith (+) $ zip xs (repeat 1)

-- insertAppend :: Ord a => a -> b -> M.Map a [b] -> M.Map a [b]
-- insertAppend k v m =
--   M.insert k (v:v') m
--   where v' = M.findWithDefault [] k m

type family Elem container where
  Elem [a]       = a
  Elem (S.Set a) = a

insertWithDefault :: Ord a => (Elem b -> b -> b) -> b -> a -> Elem b -> M.Map a b -> M.Map a b
insertWithDefault f d k v m =
  M.insert k (f v v') m
  where v' = M.findWithDefault d k m

-- | Safe list index
(!?) :: [a] -> Int -> Maybe a
xs !? i
  | i >= length xs = Nothing
  | otherwise     = Just $ xs !! i

infixl 9 !?

-- Run a function n times
replicate' :: Int -> (a -> a) -> (a -> a)
replicate' n f = foldr (.) id (replicate n f)

-- Run a monadic effect n times
replicateM' :: Monad m => Int -> (a -> m a) -> (a -> m a)
replicateM' n action = foldr (<=<) return (replicate n action)

removeSame :: Ord a => [a] -> [a]
removeSame = concat . filter ((== 1) . length) . group . sort

findSame :: Ord a => [a] -> [a]
findSame = map head . filter ((> 1) . length) . group . sort
