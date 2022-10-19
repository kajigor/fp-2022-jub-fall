{-# LANGUAGE InstanceSigs #-}
module SortedList where
import Data.List

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
-- Ещё перепишу этот кусок кода, тут можно эффективнее
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) list1 list2 = if getSortedList list1 == [] then SortedList { getSortedList = getSortedList list2 }
                     else if getSortedList list2 == [] then SortedList { getSortedList = getSortedList list1 }
                     else SortedList { getSortedList = sort (getSortedList list1 ++ getSortedList list2) }

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList { getSortedList = [] }
