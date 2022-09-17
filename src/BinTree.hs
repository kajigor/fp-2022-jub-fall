module BinTree where

data BinTree a = Leaf a -- Лист, содержащий значение
               | Node a (BinTree a) (BinTree a) -- Узел со значением и двумя потомками
               deriving (Show, Eq)

-- Возвращает элемент в корне дерева
root :: BinTree a -> a
root (Leaf a) = a
root (Node a c1 c2) = a

-- Возвращает листья дерева, перечисленные слева направо
leaves :: BinTree a -> [a]
leaves (Leaf a) = [a]
leaves (Node a c1 c2) = (leaves(c1)) ++ (leaves(c2))

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Leaf a) = [a]
nodes (Node a c1 c2) = (nodes(c1)) ++ [a] ++ (nodes(c2))

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Leaf a) = 1
depth (Node a c1 c2) = 1 + (max (depth(c1)) (depth(c2)))

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (Node a c1 c2) = Node (f a) (mapTree f c1) (mapTree f c2)


--  data List a = Empty -- пустой список без элементов, a.k.a. []
--              | AtLeastOne a (List a) -- список, у которого есть голова типа a и хвост-список, a.k.a. :
--              deriving (Show)

--  -- Применяем функцию f к каждому элементу списка
--  map' :: (a -> b) -> List a -> List b
--  map' _ Empty = Empty
--  map' f (AtLeastOne x xs) = AtLeastOne (f x) (map' f xs)

