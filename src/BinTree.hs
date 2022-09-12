{-# LANGUAGE DeriveFunctor #-}
module BinTree ( root
               , leaves
               , nodes
               , depth
               , mapTree
               , BinTree(..)
               ) where

data BinTree a = Leaf a -- Лист, содержащий значение
               | Node a (BinTree a) (BinTree a) -- Узел со значением и двумя потомками
               deriving (Show, Eq, Functor)

-- Возвращает элемент в корне дерева
root :: BinTree a -> a
root (Leaf x) = x
root (Node x _ _) = x

-- Возвращает листья дерева, перечисленные слева направо
leaves :: BinTree a -> [a]
leaves (Leaf x) = [x]
leaves (Node _ left right) = leaves left ++ leaves right

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Leaf x) = [x]
nodes (Node x left right) = nodes left ++ [x] ++ nodes right

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Leaf _) = 1
depth (Node _ left right) = 1 + max (depth left) (depth right)

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree = fmap
