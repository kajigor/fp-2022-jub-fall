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
  (<>) (PointD x1 y1) (PointD x2 y2) = (PointD (x1 + x2) (y1 + y2))

-- Фигуры
data Shape = Circle PointT Double    -- Круг характеризуется координатой центра и радиусом
           | Rectangle PointT PointT -- Прямоугольник со сторонами, параллельными координатным осям, характеризуется координатами двух углов
           | Overlay Shape Shape     -- Фигура, получающаяся наложением друг на друга двух других фигур
           deriving (Show, Eq)

-- Передвигает фигуру на x по горизонтали и на y по вертикали
-- Реализовать, используя то, что PointT -- моноид
slideShape :: Shape -> PointT -> Shape
slideShape (Circle center radius) shift = (Circle (center <> shift) radius)
slideShape (Rectangle corner1 corner2) shift = (Rectangle (corner1 <> shift) (corner2 <> shift))
slideShape (Overlay shape1 shape2) shift = (Overlay (slideShape shape1 shift) (slideShape shape2 shift))

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround shape [] = shape
moveShapeAround shape (x : xs) = moveShapeAround (slideShape shape x) xs

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.
-- Операция Overlay
instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) shape1 shape2 = Overlay shape1 shape2

-- Непонятно, какой должен быть нейтральный элемент. Скорее всего фигура, являющяяся всей плоскостью, 
-- но такую нужно дополнительно добавлять в Shape (то есть для теккущего Shape не выделить mempty)
instance Monoid Shape where
  mempty = undefined
