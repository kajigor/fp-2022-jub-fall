module BinTree where

data BinTree a = Leaf a -- Лист, содержащий значение
               | Node a (BinTree a) (BinTree a) -- Узел со значением и двумя потомками
               deriving (Show, Eq)

-- Возвращает элемент в корне дерева
root :: BinTree a -> a
root (Leaf a) = a
root (Node a _ _) = a

-- Возвращает листья дерева, перечисленные слева направо
leaves :: BinTree a -> [a]
leaves (Leaf a) = [a]
leaves (Node a leftSubtree rightSubtree) = leaves leftSubtree ++ leaves rightSubtree

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Leaf a) = [a]
nodes (Node a leftSubtree rightSubtree) = nodes leftSubtree ++ [a] ++ nodes rightSubtree

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Leaf a) = 1
depth (Node a leftSubtree rightSubtree) = 1 + max (depth leftSubtree) (depth rightSubtree)

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Node a leftSubtree rightSubtree) = Node (f a) (mapTree f leftSubtree) (mapTree f rightSubtree)
