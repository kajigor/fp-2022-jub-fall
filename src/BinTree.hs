module BinTree where

data BinTree a = Leaf a -- Лист, содержащий значение
               | Node a (BinTree a) (BinTree a) -- Узел со значением и двумя потомками
               deriving (Show, Eq)

-- Возвращает элемент в корне дерева
root :: BinTree a -> a
root (Leaf x) = x
root (Node x _ _) = x

-- Возвращает листья дерева, перечисленные слева направо
leaves :: BinTree a -> [a]
leaves (Leaf x)= [x]
leaves (Node x l r) = (leaves l) ++ (leaves r)

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Leaf x) = [x]
nodes (Node x l r) = (nodes l) ++ [x] ++ (nodes r)

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Leaf x) = 1
depth (Node x l r) = (max (depth l) (depth r)) + 1

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node x l r) = (Node (f x) (mapTree f l) (mapTree f r))
