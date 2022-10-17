{-# LANGUAGE InstanceSigs #-}
module SortedList where
import Data.List (sort)

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) (SortedList list1) (SortedList list2) = SortedList $ sort (list1 ++ list2) 

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty =  SortedList [] 