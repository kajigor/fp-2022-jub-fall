{-# LANGUAGE InstanceSigs, BangPatterns #-}
module SortedList where
import Data.List (sort)

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (SortedList s1) <> (SortedList s2) =
      SortedList $ merge s1 s2
    where
      merge [] s2 = s2
      merge s1 [] = s1
      merge (h1:t1) (h2:t2) | h1 < h2 = h1 : merge t1 (h2:t2)
      merge (h1:t1) (h2:t2) = h2 : merge (h1:t1) t2


instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []
  