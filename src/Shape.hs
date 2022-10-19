{-# LANGUAGE InstanceSigs #-}
module Shape where

-- Точка на плоскости
data PointT = PointD Double Double
            deriving (Show, Eq)

-- В современных версиях ghc Monoid уже не содержит бинарную операцию mappend,
-- mappend живет в классе Semigroup и называется <>.
-- Любой инстанс моноида должен быть инстансом Semigroup (Semigroup a => Monoid a),
-- а операция <> и mempty подчиняется тем же законам:
-- * mempty <> a = a
-- * a <> mempty = a
-- * a <> (b <> c) = (a <> b) <> c
-- * mconcat foldr (<>) mempty
instance Monoid PointT where
  mempty :: PointT
  mempty = PointD 0 0

instance Semigroup PointT where
  (<>) :: PointT -> PointT -> PointT
  (<>) (PointD x1 y1) (PointD x2 y2) = PointD (x1 + x2) (y1 + y2)

-- Фигуры
data Shape = Circle PointT Double    -- Круг характеризуется координатой центра и радиусом
           | Rectangle PointT PointT -- Прямоугольник со сторонами, параллельными координатным осям, характеризуется координатами двух углов
           | Overlay Shape Shape     -- Фигура, получающаяся наложением друг на друга двух других фигур
           deriving (Show, Eq)

-- Передвигает фигуру на x по горизонтали и на y по вертикали
-- Реализовать, используя то, что PointT -- моноид
slideShape :: Shape -> PointT -> Shape
slideShape (Circle center radius) shift = Circle (center <> shift) radius
slideShape (Rectangle vertex1 vertex2) shift = Rectangle (vertex1 <> shift) (vertex2 <> shift)
slideShape (Overlay shape1 shape2) shift = Overlay (slideShape shape1 shift) (slideShape shape2 shift)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround shape shifts = slideShape shape (mconcat shifts)

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.
-- в качестве операции <> возьмём наложение двух фигур, упорядочив взятие Overlay
-- таким образом, чтобы выполнялась ассоциативность операции <>
instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) shape1 shape2 = assembleTree (disassembleTree (Overlay shape1 shape2)) where
    disassembleTree :: Shape -> [Shape]
    disassembleTree (Overlay shape1 shape2) = disassembleTree shape1 ++ disassembleTree shape2
    disassembleTree shape = [shape]
    assembleTree :: [Shape] -> Shape
    assembleTree (x:xs@(_:_)) = Overlay x (assembleTree xs)
    assembleTree [shape] = shape

-- Shape не может быть моноидом: Наложение любой фигуры к существующей путём Overlay даст результат,
-- отличный от оригинальной фигуры. Если же считать, что наложение фигуры нулевого/бесконечного размера
-- (в зависимости от того, считаем мы наложение пересечекающим или объединяющим), то таких фигур,
-- не меняющих фигуру, на которую их накладывают, по крайней мере две - круг нулевого/бесконечного радиуса
-- и прямоугольник нулевого/бесконечного размера, что противоречит условию единственности нейтрального элемента
instance Monoid Shape where
  mempty = undefined
