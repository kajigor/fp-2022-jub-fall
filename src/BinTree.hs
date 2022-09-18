module BinTree where

data BinTree a = Leaf a -- Лист, содержащий значение
               | Node a (BinTree a) (BinTree a) -- Узел со значением и двумя потомками
               deriving (Show, Eq)

-- Возвращает элемент в корне дерева
root :: BinTree a -> a
root (Leaf el) = el
root (Node el _ _) = el

-- Возвращает листья дерева, перечисленные слева направо
leaves :: BinTree a -> [a]
leaves (Node _ left right) = leaves left ++ leaves right
leaves (Leaf el) = [el]

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Node el left right) = (nodes left ++ [el]) ++ nodes right
nodes (Leaf el) = [el]

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Node _ left right) = 1 + max (depth left) (depth right)
depth (Leaf _) = 1

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Leaf el) = Leaf (f el)
mapTree f (Node el left right) = Node (f el) (mapTree f left) (mapTree f right)

