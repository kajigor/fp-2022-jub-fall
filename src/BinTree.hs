module BinTree where

data BinTree a = Leaf a -- Лист, содержащий значение
               | Node a (BinTree a) (BinTree a) -- Узел со значением и двумя потомками
               deriving (Show, Eq)

-- Возвращает элемент в корне дерева
root :: BinTree a -> a
root (Node x _ _) = x
root (Leaf x) = x

-- Возвращает листья дерева, перечисленные слева направо
leaves :: BinTree a -> [a]
leaves (Leaf x) = [x]
leaves (Node _ a b) = leaves a ++ leaves b

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Leaf x) = [x]
nodes (Node x a b) =  nodes a ++ [x] ++ nodes b

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Leaf x) = 1
depth (Node _ a b) = (depth a) `max` (depth b) + 1


-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node x a b) = Node (f x) (mapTree f a) (mapTree f b)
