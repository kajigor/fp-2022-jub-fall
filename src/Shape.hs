{-# LANGUAGE InstanceSigs #-}
module Shape where
import GHC.Real (infinity)

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
slideShape (Rectangle c1 c2) shift = Rectangle (c1 <> shift) (c2 <> shift)
slideShape (Overlay s1 s2) shift = Overlay (slideShape s1 shift) (slideShape s2 shift)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround = foldl slideShape

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.
-- считаем, что Overlay s1 s2 == Overlay s2 s1
instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) s1 s2 = Overlay s1 s2

-- Если overlay -- перечение, нужно делать либо круг с бесконечным радиусом, либо прямоугольник с бесконечными координатами.
-- если объежинение, то нужно делать лиюо круг с 0 радиусом, либо прямоугольник нулевой площади.\
-- В любом случае, невозможно сделать так, чтобы identiny был единственным. Причем, будет нарушаться mempty <> a = a, так как Overlay и Circle -- выглядит, как что-то разное.
instance Monoid Shape where
  mempty = undefined
