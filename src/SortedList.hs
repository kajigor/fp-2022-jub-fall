{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) list1 list2 = SortedList {getSortedList = joinSorted (getSortedList list1) (getSortedList list2)}

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList {getSortedList = []}

joinSorted :: Ord a => [a] -> [a] -> [a]
joinSorted xs [] = xs
joinSorted [] ys = ys
joinSorted (x:xs) (y:ys) =
  if x < y
    then x : joinSorted xs (y:ys)
    else y : joinSorted (x:xs) ys