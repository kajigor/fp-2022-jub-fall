module BinTree where

data BinTree a = Leaf a -- Лист, содержащий значение
               | Node a (BinTree a) (BinTree a) -- Узел со значением и двумя потомками
               deriving (Show, Eq)

-- Возвращает элемент в корне дерева
root :: BinTree a -> a
root (Node a l r) = a
root (Leaf a) = a

-- Возвращает листья дерева, перечисленные слева направо
merge :: [a] -> [a] -> [a]
merge [] b = b
merge (x : xs) b =  x : merge xs b
leaves :: BinTree a -> [a]
leaves (Node n l r) = merge (leaves l) (leaves r)
leaves (Leaf a) = [a]
leaves _ = []

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Node n l r) = merge (merge (nodes l) [n]) (nodes r)
nodes (Leaf a) = [a]
nodes _ = []

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Node a l r) = max (1+ depth l) (1 + depth r)
depth (Leaf a) = 1
depth _ = 0

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Node a l r) = (Node (f a) (mapTree f l) (mapTree f r))
mapTree f (Leaf a) = Leaf (f a)
mapTree _ _ = undefined

