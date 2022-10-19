{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) l1 l2 = SortedList {
    getSortedList = merging (getSortedList l1) (getSortedList l2) }
    where
      merging element1 [] = element1
      merging [] element2 = element2
      merging (x : xs) (y : ys) =
          if (x >= y) then y : (merging (x : xs) ys)
          else x : (merging (y : ys) xs)

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList { getSortedList = [] }