module BinTree where

data BinTree a = Leaf a -- Лист, содержащий значение
               | Node a (BinTree a) (BinTree a) -- Узел со значением и двумя потомками
               deriving (Show, Eq)

-- Возвращает элемент в корне дерева
root :: BinTree a -> a
root (Leaf x) = x
root (Node x _ _) = x

-- Добавляет листья дерева в начало переданного списка
addLeavesToList :: BinTree a -> [a] -> [a]
addLeavesToList (Leaf leaf) list = leaf : list
addLeavesToList (Node _ left right) list = addLeavesToList left (addLeavesToList right list)

-- Возвращает листья дерева, перечисленные слева направо
leaves :: BinTree a -> [a]
leaves tree = addLeavesToList tree []

-- Добавляет узлы дерева в начало переданного списка
addNodesToList :: BinTree a -> [a] -> [a]
addNodesToList (Leaf leaf) list = leaf : list
addNodesToList (Node value left right) list = addNodesToList left (value : addNodesToList right list)

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes tree = addNodesToList tree []

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Leaf _) = 1
depth (Node _ left right) = 1 + max (depth left) (depth right)

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree func (Leaf x) = Leaf (func x)
mapTree func (Node value left right) = Node (func value) (mapTree func left) (mapTree func right)
