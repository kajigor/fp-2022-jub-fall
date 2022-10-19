{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) list1 list2 = SortedList { getSortedList = mergeSorted (getSortedList list1) (getSortedList list2) } where
    mergeSorted [] ys = ys
    mergeSorted xs []= xs
    mergeSorted (x : xs) (y : ys)
        | x < y     = x : mergeSorted xs (y : ys)
        | otherwise = y : mergeSorted (x : xs) ys 

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList { getSortedList = [] }
