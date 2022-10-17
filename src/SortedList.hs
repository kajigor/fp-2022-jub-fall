{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) a (SortedList []) = a
  (<>) (SortedList []) b = b
  (<>) (SortedList (x:xs)) (SortedList (y:ys)) = 
    if x < y
    then let ~(SortedList zs) = SortedList xs <> SortedList (y:ys) in
      SortedList (x : zs)
    else let ~(SortedList zs) = SortedList (x:xs) <> SortedList ys in
      SortedList (y : zs)

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []