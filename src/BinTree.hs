module BinTree where

data BinTree a = Leaf a -- Лист, содержащий значение
               | Node a (BinTree a) (BinTree a) -- Узел со значением и двумя потомками
               deriving (Show, Eq)

-- Возвращает элемент в корне дерева
root :: BinTree a -> a
root (Leaf v) = v
root (Node v _ _) = v

-- Возвращает листья дерева, перечисленные слева направо
leaves :: BinTree a -> [a]
leaves (Leaf v) = [v]
leaves (Node _ left right) = leaves left ++ leaves right

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Leaf v) = [v]
nodes (Node v left right) = nodes left ++ [v] ++ nodes right

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Leaf _) = 1
depth (Node _ left right) = max (depth left) (depth right) + 1

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree func (Leaf v) = Leaf (func v)
mapTree func (Node v left right) = Node (func v) (mapTree func left) (mapTree func right)

