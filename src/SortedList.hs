{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) (SortedList []) (SortedList []) = SortedList []
  (<>) l1 (SortedList []) = l1
  (<>) (SortedList []) l2 = l2
  (<>) l1@(SortedList (h1:t1)) l2@(SortedList (h2:t2)) =
    if h1 < h2
      then SortedList (h1 : getSortedList (SortedList t1 <> l2))
      else SortedList (h2 : getSortedList (l1 <> SortedList t2))

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []