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
leaves (Leaf l) = [l]
leaves (Node _ leftTree rightTree) = leaves leftTree ++ leaves rightTree

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Leaf l) = [l]
nodes (Node a leftTree rightTree) = nodes leftTree ++ [a] ++ nodes rightTree

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Leaf _) = 1
depth (Node _ leftTree rightTree) = 1 + max (depth leftTree) (depth rightTree)

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Leaf l) = Leaf (f l)
mapTree f (Node a leftTree rightTree) = Node (f a) (mapTree f leftTree) (mapTree f rightTree)
