module BinTree where

data BinTree a = Leaf a -- Лист, содержащий значение
               | Node a (BinTree a) (BinTree a) -- Узел со значением и двумя потомками
               deriving (Show, Eq)

-- Возвращает элемент в корне дерева
root :: BinTree a -> a
root tree = case tree of
  Leaf x -> x
  Node x _ _ -> x

-- Возвращает листья дерева, перечисленные слева направо
leaves :: BinTree a -> [a]
leaves tree = case tree of
  Leaf x -> [x]
  Node x left right -> (leaves left) ++ (leaves right)

-- Возвращает узлы дерева, перечисленные слева направо, сверху вниз
nodes :: BinTree a -> [a]
nodes tree = case tree of
  Leaf x -> [x]
  Node x left right -> (nodes left) ++ [x] ++ (nodes right)

-- Глубина дерева -- длина пути до самого глубокого листа
-- Глубина дерева из одного листа -- 1
depth :: BinTree a -> Int
depth tree = case tree of
  Leaf _ -> 1
  Node x left right -> (max (depth left) (depth right)) + 1

-- Применяет функцию ко всем элементам дерева
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f tree = case tree of
  Leaf x -> Leaf (f x)
  Node x left right -> Node (f x) (mapTree f left) (mapTree f right)
