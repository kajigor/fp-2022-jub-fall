{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) SortedList { getSortedList = [] } xs = xs
  (<>) xs SortedList { getSortedList = [] } = xs
  (<>) SortedList { getSortedList = x:xs } SortedList { getSortedList = y:ys } 
    | x < y = SortedList { getSortedList = x : getSortedList (SortedList { getSortedList = xs } <> SortedList { getSortedList = y:ys })}
    | otherwise = SortedList { getSortedList = y : getSortedList (SortedList { getSortedList = x:xs } <> SortedList { getSortedList = ys })}

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList { getSortedList = [] }
  