{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) s1 s2 = case (getSortedList s1, getSortedList s2) of
    ([], []) -> SortedList []
    (l, []) -> SortedList l
    ([], l) -> SortedList l
    (l1@(head1:tail1), l2@(head2:tail2)) -> if (head1 < head2) then SortedList (head1 : getSortedList (SortedList tail1 <> SortedList l2)) 
                                            else SortedList (head2 : getSortedList (SortedList l1 <> SortedList tail2))

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []