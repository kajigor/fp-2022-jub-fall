{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) a b = SortedList (cat (getSortedList a) (getSortedList b)) where
    cat :: Ord t => [t] -> [t] -> [t]
    cat as [] = as
    cat [] bs = bs
    cat (a:as) (b:bs) | a < b = a : (cat as (b:bs))
                      | otherwise = b : (cat (a:as) bs)

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []