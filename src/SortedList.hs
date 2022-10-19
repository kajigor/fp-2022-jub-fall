{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) arr0 arr1 = SortedList {getSortedList = join (getSortedList arr0) (getSortedList arr1)}

join :: Ord a => [a] -> [a] -> [a]
join x [] = x
join [] y = y
join (x : xs) (y : ys)
  | x > y = (y : join (x : xs) ys)
  | otherwise = (x : join xs (y : ys))

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []
