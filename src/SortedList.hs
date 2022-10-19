{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) a b = SortedList (merge (getSortedList a) (getSortedList b))
    where merge as [] = as
          merge [] bs = bs
          merge (a : as) (b : bs) | a < b = a : (merge as (b : bs))
                                  | otherwise = b : (merge (a : as) bs)

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []