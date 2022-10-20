{-# LANGUAGE InstanceSigs #-}
module SortedList where
import Data.List (sort)

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) (SortedList {getSortedList = list1}) (SortedList {getSortedList = list2}) = SortedList {getSortedList = sortLists list1 list2}  


sortLists :: (Ord a) => [a] -> [a] -> [a]
sortLists list1 [] = list1
sortLists [] list2 = list2
sortLists (l1:list1) (l2:list2) | l1 > l2 = l2 : (sortLists (l1:list1) list2)
                                | otherwise = l1 : (sortLists list1 (l2:list2))
instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty =  SortedList [] 