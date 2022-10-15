{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) sl1 sl2 = case (getSortedList sl1, getSortedList sl2) of
    (l, []) -> SortedList l
    ([], l) -> SortedList l
    (list1@(h1:xs1), list2@(h2:xs2)) | h1 < h2 -> SortedList $ h1 : (getSortedList $ SortedList xs1 <> SortedList list2)
                                     | otherwise -> SortedList $ h2 : (getSortedList $ SortedList list1 <> SortedList xs2)

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []