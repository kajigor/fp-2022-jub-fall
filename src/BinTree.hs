module BinTree where

data BinTree a = Leaf a -- Лист, содержащий значение
               | Node a (BinTree a) (BinTree a) -- Узел со значением и двумя потомками
               deriving (Show, Eq)

-- Возвращает элемент в корне дерева
root :: BinTree a -> a
root (Node x _ _) = x
root (Leaf x) = x
-- root _ = undefined

-- Возвращает листья дерева, перечисленные слева направо
leaves :: BinTree a -> [a]
leaves (Node _ l_tree r_tree) = (leaves l_tree) ++ (leaves r_tree)
leaves (Leaf x) = [x]
-- leaves _ = undefined

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Node x l_tree r_tree) = (nodes l_tree) ++ [x] ++ (nodes r_tree)
nodes (Leaf x) = [x]
-- nodes _ = undefined

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Node x l_tree r_tree) = 1 + (max (depth l_tree) (depth r_tree))
depth (Leaf x) = 1
-- depth _ = undefined

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Node x l_tree r_tree) = Node (f x) (mapTree f l_tree) (mapTree f r_tree)
mapTree f (Leaf x) = Leaf (f x)
-- mapTree _ _ = undefined

