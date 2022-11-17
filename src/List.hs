{-# LANGUAGE InstanceSigs #-}
module List where

-- Класс типов для контейнеров, в которых можно найти минимальный и максимальный элемент
class Ranged t where
  maximumValue :: Ord a => t a -> a
  minimumValue :: Ord a => t a -> a

-- Обычные списки мы просто обходим и ищем максимальное и минимальное значения в процессе
instance Ranged [] where
  maximumValue xs =
      go (head xs) (tail xs)
    where
      go curMax [] = curMax
      go curMax (h : t) | h > curMax = go h t
                        | otherwise = go curMax t

  minimumValue xs =
      go (head xs) (tail xs)
    where
      go curMax [] = curMax
      go curMax (h : t) | h < curMax = go h t
                        | otherwise = go curMax t

-- Тип данных для отсортированных списков
newtype SortedList a = Sorted { getSorted :: [a] } deriving (Show, Eq)

-- Для отсортированных списков минимальное и максимальное значения всегда на концах списка
instance Ranged SortedList where
  maximumValue = last . getSorted
  minimumValue = head . getSorted

-- Эталонная реализация обращения списка, для которой очевидно, что она работает корректно
reverseList :: [a] -> [a]
reverseList [] = []
reverseList (h : t) = reverseList t ++ [h]

-- Более эффективная реализация обращения списка
fastReverseList :: [a] -> [a]
fastReverseList =
    go []
  where
    go acc [] = acc
    go acc (h : t) = go (h : acc) t