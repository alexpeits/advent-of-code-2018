{-# LANGUAGE RecordWildCards, QuasiQuotes #-}
module Advent.Day04 where

import Data.List (sort, sortOn)
import Control.Monad.State.Strict
import qualified Data.Map as M

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import Text.RawString.QQ

import Advent.Util

day :: Int
day = 4

type GuardId = Int
type Year    = Int
type Month   = Int
type Day     = Int
type Hour    = Int
type Minute  = Int

data Record = MkRecord
  { rDate :: Date
  , rLog  :: Log
  }
  deriving Show

instance Eq Record where
  MkRecord d1 _ == MkRecord d2 _ = d1 == d2

instance Ord Record where
  MkRecord d1 _ <= MkRecord d2 _ = d1 <= d2

data Date = MkDate
  { dateY :: Year
  , dateM :: Month
  , dateD :: Day
  , dateh :: Hour
  , datem :: Minute
  } deriving (Eq, Ord, Show)

data Log = Begin GuardId | Sleep | Wake deriving Show

parseRecord :: Parser Record
parseRecord = do
  char '['
  rDate <- parseDate
  char ']' >> space
  rLog <- parseLog
  return MkRecord{..}

parseDate :: Parser Date
parseDate = do
  dateY <- parseNum
  char '-'
  dateM <- parseNum
  char '-'
  dateD <- parseNum
  char ' '
  dateh <- parseNum
  char ':'
  datem <- parseNum
  return MkDate{..}

parseLog :: Parser Log
parseLog = parseBegin <|> parseSleep <|> parseWake
  where parseBegin = Begin <$>
          (string "Guard #" *> parseNum <* string " begins shift")
        parseSleep = string "falls asleep" >> pure Sleep
        parseWake  = string "wakes up" >> pure Wake

minuteRange :: Date -> Date -> [Minute]
minuteRange MkDate{datem = m1} MkDate{datem = m2} = [m1 .. m2 - 1]

data StState = StAsleep {asleepSince :: Date} | StAwake
data St = MkSt
  { stGuard :: GuardId
  , stState :: StState
  , stLog   :: M.Map GuardId [Minute]
  }

initSt :: St
initSt = MkSt 0 StAwake M.empty

step :: Record -> State St ()
step (MkRecord cd cl) = do
  MkSt g s l <- get
  let rng = minuteRange (asleepSince s) cd
      newL = M.insertWith (++) g rng l
  case cl of
    Begin gid -> put $ MkSt gid StAwake l
    Sleep     -> put $ MkSt g (StAsleep cd) l
    Wake      -> put $ MkSt g StAwake newL

buildLog :: [Record] -> M.Map GuardId [Minute]
buildLog rs = stLog $ execState (mapM step rs) initSt

part1 :: [Record] -> Int
part1 rs = maxGuard * maxMinute
  where logMap = M.toList $ buildLog rs
        (maxGuard, mins) = last $ sortOn (length . snd) logMap
        maxMinute = fst $ last $ sortOn snd $ M.toList $ counter mins

part2 :: [Record] -> Int
part2 rs = maxGuard * maxMinute
  where logMap = buildLog rs
        freqMap = M.toList $ M.map (last . sortOn snd . M.toList . counter) logMap
        (maxGuard, (maxMinute, _)) = last $ sortOn (snd . snd) freqMap

main :: IO ()
main = do
  rawInput <- readInputLines day
  let (Right input) = sort <$> mapM (parse parseRecord "") rawInput
  printResults day (part1 input) (part2 input)
