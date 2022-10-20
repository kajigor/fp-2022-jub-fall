{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) s0 s1 = SortedList (merge (getSortedList s0) (getSortedList s1))

merge :: Ord a => [a] -> [a] -> [a]
merge [] a = a
merge a [] = a
merge (a0 : tail0) (a1 : tail1) = if a0 < a1 then a0 : merge tail0 (a1 : tail1)
                                            else a1 : merge (a0 : tail0) tail1

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []