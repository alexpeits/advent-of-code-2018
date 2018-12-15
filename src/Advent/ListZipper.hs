{-# LANGUAGE PatternSynonyms #-}
module Advent.ListZipper where
-- | Very basic list zipper

import Data.Maybe (fromJust, isJust)

import Advent.Util (replicate', replicateM')

class IsZipper z where
  fromList :: [a] -> z a
  toList   :: z a -> [a]
  mirror   :: z a -> z a

  peekLeft :: z a -> Maybe a
  forward  :: z a -> Maybe (z a)
  insertLeft :: a -> z a -> z a
  removeLeft :: z a -> Maybe (a, z a)

  hasLeft  :: z a -> Bool
  hasLeft = isJust . peekLeft

hasRight :: IsZipper z => z a -> Bool
hasRight = hasLeft . mirror

peekRight :: IsZipper z => z a -> Maybe a
peekRight = peekLeft . mirror

backward :: IsZipper z => z a -> Maybe (z a)
backward = fmap mirror . forward . mirror

insertRight :: IsZipper z => a -> z a -> z a
insertRight x = mirror . insertLeft x . mirror

removeRight :: IsZipper z => z a -> Maybe (a, z a)
removeRight = (fmap . fmap) mirror . removeLeft . mirror

-- Helpers for rotating n times
forwardN :: IsZipper z => Int -> z a -> Maybe (z a)
forwardN n = replicateM' n forward

backwardN :: IsZipper z => Int -> z a -> Maybe (z a)
backwardN n = fmap mirror . forwardN n . mirror

-- Unsafe versions of some operations
forward' :: IsZipper z => z a -> z a
forward' = fromJust . forward

backward' :: IsZipper z => z a -> z a
backward' = fromJust . backward

forwardN' :: IsZipper z => Int -> z a -> z a
forwardN' n = replicate' n forward'

backwardN' :: IsZipper z => Int -> z a -> z a
backwardN' n = mirror . forwardN' n . mirror

removeLeft' :: IsZipper z => z a -> (a, z a)
removeLeft' = fromJust . removeLeft

removeRight' :: IsZipper z => z a -> (a, z a)
removeRight' = fromJust . removeRight

data ListZipper a =
  MkListZipper [a] a [a]
  deriving (Eq, Show)

instance IsZipper ListZipper where
  fromList (x:xs) = MkListZipper [] x xs
  toList (MkListZipper l f r) = reverse l ++ (f:r)
  mirror (MkListZipper l f r) = MkListZipper r f l

  peekLeft (MkListZipper []    _ _) = Nothing
  peekLeft (MkListZipper (x:_) _ _) = Just x

  forward (MkListZipper _  _ [])     = Nothing
  forward (MkListZipper l  f (x:xs)) = Just $ MkListZipper (f:l) x xs

  insertLeft x (MkListZipper l f r) = MkListZipper (x:l) f r
  removeLeft (MkListZipper [] _ _) = Nothing
  removeLeft (MkListZipper (x:xs) f r) = Just (x, MkListZipper xs f r)

-- Cyclical ListZipper
newtype ListZipperC a = MkListZipperC_
  { getListZipper :: ListZipper a }
  deriving (Eq, Show)

pattern MkListZipperC l f r = MkListZipperC_ (MkListZipper l f r)

instance IsZipper ListZipperC where
  fromList = MkListZipperC_ . fromList
  toList = toList . getListZipper
  mirror = MkListZipperC_ . mirror . getListZipper

  peekLeft (MkListZipperC []    f []) = Just f
  peekLeft (MkListZipperC (x:_) _ _ ) = Just x
  peekLeft (MkListZipperC []    _ l ) = Just $ last l

  -- Auto-balancing when exhausted
  forward z@(MkListZipperC [] f []) = Just z
  forward   (MkListZipperC l  f []) = Just $ MkListZipperC (f:l') f' r
    where (l', rr) = splitAt ((length l `div` 2) - 1) l
          (f':r) = reverse rr
  forward z = MkListZipperC_ <$> forward (getListZipper z)

  insertLeft x = MkListZipperC_ . insertLeft x . getListZipper

  removeLeft (MkListZipperC [] f []) = Nothing
  removeLeft (MkListZipperC [] f r ) = Just (last r, MkListZipperC [] f (init r))
  removeLeft z = fmap MkListZipperC_ <$> removeLeft (getListZipper z)

  -- A cyclic zipper always has an item in its left
  -- (If only one item, then it is itself the item in its left)
  hasLeft = const True
