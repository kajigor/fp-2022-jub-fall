{-# LANGUAGE InstanceSigs #-}

module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a =
  SortedList
    { getSortedList :: [a]
    }
  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) a b =
    case (getSortedList a, getSortedList b) of
      ([], []) -> SortedList []
      (nempty, []) -> SortedList nempty
      ([], nempty) -> SortedList nempty
      ((lh:lt), (rh:rt)) ->
        case lh < rh of
          True ->
            SortedList
              (lh : getSortedList (SortedList lt <> SortedList (rh : rt)))
          False ->
            SortedList
              (rh : getSortedList (SortedList rt <> SortedList (lh : lt)))

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []
