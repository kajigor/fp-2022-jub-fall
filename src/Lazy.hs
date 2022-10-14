{-# LANGUAGE StrictData #-} -- Все типы данных в этом модуле будут строгими

module Lazy where

data Tree a = Leaf a
            | Node a ~(Tree a) ~(Tree a) -- Тильда делает поддеревья ленивыми; значение в узле -- строгое
            deriving Show

data Stream x = x :| Stream x -- Строгий бесконечный список
              deriving Show

-- бесконечное дерево
infinitree :: t -> Tree t
infinitree x = Node x (infinitree x) (infinitree x)

-- Если убрать тильды в определении дерева, leaf (infinitree 0) не терминируется,
-- потому что сначала пытается построить целиком бесконечное дерево
leaf :: Tree a -> Maybe a
leaf (Leaf x) = Just x
leaf (Node {}) = Nothing

root :: Tree a -> Maybe a
root (Leaf x) = Just x
root (Node x _ _) = Just x

-- Строим бесконечный список значений x
makeStream :: a -> Stream a
makeStream x = x :| makeStream x

-- leaf (t 0) не завершится, несмотря на тильды у поддеревьев,
-- потому что форсит бесконечный список в узле.
t :: a -> Tree (Stream a)
t x = Node (makeStream x) (t x) (t x)

-- Для ленивых бесконечных структур данных удобно иметь функцию на подобие prune,
-- Которая берет некоторый конечный префикс
prune :: Tree a -> Int -> Tree a
prune (Node x l r) n =
  if n == 0
  then Leaf x
  else Node x (prune l $ n - 1) (prune r $ n - 1)
prune x _ = x

