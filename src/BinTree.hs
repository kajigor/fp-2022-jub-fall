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
leaves (Node a l1 l2) = leaves l1 ++ leaves l2

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Leaf val) = [val]
nodes (Node val l1 l2) = nodes l1 ++ [val] ++ nodes l2



-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Node _ l1 l2) = 1 + max (depth l1) (depth l1)
depth (Leaf _) = 1

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Node val  x1  x2) = Node (f val)  (mapTree f x1)  (mapTree f x2)
mapTree f (Leaf val) = Leaf (f val)
