module BinTree where

data BinTree a = Leaf a -- Лист, содержащий значение
               | Node a (BinTree a) (BinTree a) -- Узел со значением и двумя потомками
               deriving (Show, Eq)

-- Возвращает элемент в корне дерева
root :: BinTree a -> a
root (Node a left right) = a
root (Leaf a) = a

-- Возвращает листья дерева, перечисленные слева направо
leaves :: BinTree a -> [a]
leaves (Node a left right) = leaves left ++ leaves right
leaves (Leaf a) = [a]

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Node a left right) = nodes left ++ [a] ++ nodes right
nodes (Leaf a) = [a]

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Node a left right) = 1 + max (depth left) (depth right)
depth (Leaf a) = 1

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Node a left right) = Node (f a) (mapTree f left) (mapTree f right)
mapTree f (Leaf a) = Leaf (f a)
