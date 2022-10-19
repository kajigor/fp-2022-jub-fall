{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) a b = SortedList { getSortedList = merge (getSortedList a) (getSortedList b) }
    where
      merge :: Ord a => [a] -> [a] -> [a]
      merge x [] = x
      merge [] y = y
      merge (x : xs) (y : ys) 
        | x <= y = x : merge xs (y : ys)
        | otherwise = y : merge (x : xs) ys

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList { getSortedList = [] }