{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module BinTree where

data BinTree a = Leaf a -- Лист, содержащий значение
               | Node a (BinTree a) (BinTree a) -- Узел со значением и двумя потомками
               deriving (Show, Eq)

-- Возвращает элемент в корне дерева
root :: BinTree a -> a
root (Leaf r) = r
root (Node r _ _) = r

-- Возвращает листья дерева, перечисленные слева направо
leaves :: BinTree a -> [a]
leaves (Leaf l) = [l]
leaves (Node n l r) = leaves l ++ leaves r 

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Leaf l) = [l]
nodes (Node n l r) = nodes l ++ [n] ++ nodes r

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Leaf l) = 1
depth (Node _ l r) = 1 + max (depth l) (depth r)

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Leaf l) = Leaf (f l) 
mapTree f (Node n l r) = Node (f n) (mapTree f l) (mapTree f r)

