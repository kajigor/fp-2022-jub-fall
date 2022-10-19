{-# LANGUAGE InstanceSigs #-}
module SortedList where

-- Тип данных, представляющий собой отсортированный список
data SortedList a = SortedList { getSortedList :: [a] }
                  deriving (Show, Eq)

-- <> должен сохранять отсортированность списка
instance Ord a => Semigroup (SortedList a) where
  (<>) :: SortedList a -> SortedList a -> SortedList a
  (<>) (SortedList (xHead:xTail)) (SortedList (yHead:yTail)) | xHead < yHead = SortedList (xHead : getSortedList (SortedList xTail <> SortedList (yHead:yTail)))
  (<>) (SortedList (xHead:xTail)) (SortedList (yHead:yTail))  = SortedList (yHead : getSortedList (SortedList (xHead:xTail) <> SortedList yTail))
  (<>) list (SortedList []) =  list
  (<>) _ list = list

instance Ord a => Monoid (SortedList a) where
  mempty :: SortedList a
  mempty = SortedList []