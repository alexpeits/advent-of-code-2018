module Advent.ListZipper where
-- | Very basic list zipper

data ListZipper a =
  MkListZipper [a] a [a]
  deriving (Eq, Show)

-- | Create a ListZipper focusing on the first
-- element of the list
fromList :: [a] -> ListZipper a
fromList (x:xs) = MkListZipper [] x xs

-- | Convert a ListZipper back into a list
toList :: ListZipper a -> [a]
toList (MkListZipper left focus right) =
  reverse left ++ (focus:right)

peekLeft :: ListZipper a -> Maybe a
peekLeft (MkListZipper []    _ _) = Nothing
peekLeft (MkListZipper (x:_) _ _) = Just x

peekRight :: ListZipper a -> Maybe a
peekRight (MkListZipper _ _ []   ) = Nothing
peekRight (MkListZipper _ _ (x:_)) = Just x

backward :: ListZipper a -> ListZipper a
backward (MkListZipper [] _ _) = error "Not implemented"
backward (MkListZipper (x:xs) focus right) =
  MkListZipper xs x (focus:right)

forward :: ListZipper a -> ListZipper a
forward (MkListZipper _ _ []) = error "Not implemented"
forward (MkListZipper left focus (x:xs)) =
  MkListZipper (focus:left) x xs
