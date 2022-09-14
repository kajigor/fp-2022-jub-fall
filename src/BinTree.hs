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
leaves (Node _ x y) = leaves x ++ leaves y

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Leaf a) = [a]
nodes (Node a x y) = nodes x ++ [a] ++ nodes y

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Leaf _) = 1
depth (Node _ x y) = max (depth x) (depth y) + 1

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree func (Leaf a) = Leaf (func a)
mapTree func (Node a x y) = Node (func a) (mapTree func x) (mapTree func y)

