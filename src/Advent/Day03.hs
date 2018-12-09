{-# LANGUAGE RecordWildCards #-}
module Advent.Day03 where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (partition)
import Control.Arrow ((***), (&&&))

import Text.Parsec
import Text.Parsec.String (Parser)

import Advent.Util


day :: Int
day = 3

type Inches = Int

data Claim = MkClaim
  { claimId       :: Int
  , claimDistLeft :: Inches
  , claimDistTop  :: Inches
  , claimWidth    :: Inches
  , claimHeight   :: Inches
  }
  deriving Show

parseClaim :: Parser Claim
parseClaim = do
  char '#'
  claimId <- parseNum
  spaces >> char '@' >> spaces
  claimDistLeft <- parseNum
  char ','
  claimDistTop <- parseNum
  char ':' >> spaces
  claimWidth <- parseNum
  char 'x'
  claimHeight <- parseNum
  return MkClaim{..}

claimCoords :: Claim -> [(Int, Int)]
claimCoords MkClaim{..} =
  [ (x, y)
  | x <- take claimWidth [claimDistLeft..]
  , y <- take claimHeight [claimDistTop..]
  ]
  where claimEndRight  = claimDistLeft + claimWidth - 1
        claimEndBottom = claimDistTop + claimHeight - 1

part1 :: [Claim] -> Int
part1 =
  length
  . filter (>= 2)
  . M.elems
  . foldl (\m coord -> M.insertWith (+) coord 1 m) M.empty
  . concatMap claimCoords

insertAppend :: Ord a => a -> b -> M.Map a [b] -> M.Map a [b]
insertAppend k v m =
  M.insert k (v:v') m
  where v' = M.findWithDefault [] k m

part2 :: [Claim] -> Int
part2 cs =
  head $ S.toList $ S.difference singles nonSingles
  where coords = map (claimCoords &&& claimId) cs
        fabric =
          foldl
            (\m (coords, cid) ->
               foldl (\m' x -> M.insertWith S.union x (S.singleton cid) m') m coords)
            M.empty
            coords
        (singles, nonSingles) =
          let f = S.unions
          in (f *** f) $ partition ((== 1) . S.size) (M.elems fabric)

main :: IO ()
main = do
  rawInput <- readInputLines day
  let (Right input) = mapM (parse parseClaim "") rawInput
  printResults day (part1 input) (part2 input)
