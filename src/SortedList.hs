{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) xs (SortedList []) = xs
  (<>) (SortedList []) xs = xs
  (<>) (SortedList (x:xs)) (SortedList (y:ys)) = if x < y
                                             then (SortedList (x:(getSortedList( (SortedList xs) <> (SortedList (y:ys))))))
                                             else (SortedList (y:(getSortedList ((SortedList (x:xs)) <> (SortedList ys)))))

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []
