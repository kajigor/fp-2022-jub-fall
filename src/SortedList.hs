{-# LANGUAGE InstanceSigs #-}

module SortedList where

import GHC.Read (list)

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList {getSortedList :: [a]}
  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) (SortedList []) l2 = l2
  (<>) l1 (SortedList []) = l1
  (<>) (SortedList (l1 : ls1)) (SortedList (l2 : ls2)) | l1 > l2 = SortedList (l2 : getSortedList (SortedList (l1 : ls1) <> SortedList ls2))
  (<>) (SortedList (l1 : ls1)) (SortedList (l2 : ls2)) = SortedList (l1 : getSortedList (SortedList ls1 <> SortedList (l2 : ls2)))

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []
