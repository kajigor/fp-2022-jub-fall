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
leaves (Node _ left right) = (leaves left)++(leaves right)

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Leaf a) = [a]
nodes (Node a left right) = (nodes left)++(a:(nodes right))

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Leaf _) = 1
depth (Node _ left right) = ((max (depth left) (depth right)) + 1)

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree func (Leaf a) = (Leaf (func a))
mapTree func (Node a left right) = (Node (func a) (mapTree func left) (mapTree func right))

