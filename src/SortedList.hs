{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) list1 list2 = case (getSortedList list1, getSortedList list2) of
    ([], []) -> mempty list1
    (a, []) -> SortedList a
    ([], b) -> SortedList b
    (a : as, b : bs) -> if (a < b) then SortedList (a : getSortedList (SortedList as <> list2))
                        else SortedList (b : getSortedList (list1 <> SortedList bs))

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []