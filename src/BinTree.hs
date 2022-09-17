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
leaves (Leaf leaf) = [leaf]
leaves (Node _ l r) = leaves l ++ leaves r

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Leaf leaf) = [leaf]
nodes (Node val l r) = nodes l ++ [val] ++ nodes r

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Leaf _) = 1
depth (Node _ l r) = max (depth l) (depth r) + 1

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree fun (Leaf leaf) = Leaf (fun leaf)
mapTree fun (Node val l r) = Node (fun val) (mapTree fun l) (mapTree fun r)

