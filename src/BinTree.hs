module BinTree where

data BinTree a
  = Leaf a -- Лист, содержащий значение
  | Node a (BinTree a) (BinTree a) -- Узел со значением и двумя потомками
  deriving (Show, Eq)

-- Возвращает элемент в корне дерева
root :: BinTree a -> a
root (Leaf r) = r
root (Node r _ _) = r

-- Возвращает листья дерева, перечисленные слева направо
leaves :: BinTree a -> [a]
leaves (Leaf r) = [r]
leaves (Node _ left right) = leaves left ++ leaves right

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes (Leaf r) = [r]
nodes (Node r left right) = nodes left ++ [r] ++ nodes right

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth (Leaf r) = 1
depth (Node r left right) = 1 + max (depth left) (depth right)

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Leaf r) = Leaf (f r)
mapTree f (Node r left right) = Node (f r) (mapTree f left) (mapTree f right)
