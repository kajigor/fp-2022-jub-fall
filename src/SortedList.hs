{-# LANGUAGE InstanceSigs #-}
module SortedList where

import Data.List

-- Тип данных, представляющий собой отсортированный список
newtype SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) sl1 sl2 = SortedList . sort $ getSortedList sl1 ++ getSortedList sl2

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []
  