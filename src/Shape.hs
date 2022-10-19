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
  (<>) (PointD x0 y0) (PointD x1 y1) = PointD (x0 + x1) (y0 + y1)

-- Фигуры
data Shape = Circle PointT Double    -- Круг характеризуется координатой центра и радиусом
           | Rectangle PointT PointT -- Прямоугольник со сторонами, параллельными координатным осям, характеризуется координатами двух углов
           | Overlay Shape Shape     -- Фигура, получающаяся наложением друг на друга двух других фигур
           deriving (Show, Eq)

-- Передвигает фигуру на x по горизонтали и на y по вертикали
-- Реализовать, используя то, что PointT -- моноид
slideShape :: Shape -> PointT -> Shape
slideShape (Circle center radius) slide = Circle (center <> slide) radius
slideShape (Rectangle corner1 corner2) slide = Rectangle (corner1 <> slide) (corner2 <> slide)
slideShape (Overlay shape1 shape2) slide = Overlay (slideShape shape1 slide) (slideShape shape2 slide)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround shape moves = slideShape shape (mconcat moves)

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.

-- Если Shape является полугруппой (=> моноидом),
-- то должна быть ассоциативная операция (пусть это будет абстрактная операция OP),
-- но можно придумать такой пример, что
-- (Overlay shape1 shape2) OP ((Overlay shape2 shape3) OP (Overlay shape1 shape3))
-- !=
-- ((Overlay shape1 shape2) OP (Overlay shape2 shape3)) OP (Overlay shape1 shape3)
instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) = undefined

instance Monoid Shape where
  mempty = undefined
