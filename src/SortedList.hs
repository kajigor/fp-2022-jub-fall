{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) list1 list2 = SortedList {
    getSortedList = (mergeSortedLists (getSortedList list1) (getSortedList list2))
  }
    where
      mergeSortedLists ::  (Ord a) => [a] -> [a] -> [a]
      mergeSortedLists [] [] = []
      mergeSortedLists [] half2 = half2
      mergeSortedLists half1 [] = half1
      mergeSortedLists (x : half1) (y : half2) = 
        if (x < y) then (x : (mergeSortedLists half1 (y : half2)))
        else (y : mergeSortedLists (x : half1) half2)


instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList {
    getSortedList = []
  }