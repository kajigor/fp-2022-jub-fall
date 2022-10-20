{-# LANGUAGE InstanceSigs #-}
module SortedList where

import Data.List(sort)

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) a b = SortedList(helpfunction (getSortedList a) (getSortedList b))
            where helpfunction [] bl = bl
                  helpfunction al [] = al
                  helpfunction (xa : al) (xb : bl) | xa < xb = [xa] ++ (helpfunction al (xb : bl))
                                                   | otherwise = [xb] ++ (helpfunction (xa : al) bl)
   
instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList {getSortedList = []}