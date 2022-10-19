{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-compat-unqualified-imports #-}
module SortedList where
import Data.List

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) a b = SortedList (sort (getSortedList a ++ getSortedList b))

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList [] 