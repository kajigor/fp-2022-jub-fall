{-# LANGUAGE InstanceSigs #-}

module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList {getSortedList :: [a]} deriving (Show, Eq)

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) (SortedList []) ys = ys
  (<>) xs (SortedList []) = xs
  (<>) sl1@(SortedList (x : xs)) sl2@(SortedList (y : _))
    | x <= y = SortedList (x : getSortedList (SortedList xs <> sl2))
    | otherwise = sl2 <> sl1
