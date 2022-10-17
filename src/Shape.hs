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
           deriving (Show)

asArrayOfPrimitives :: Shape -> [Shape]
asArrayOfPrimitives (Overlay shape1 shape2) = (asArrayOfPrimitives shape1) ++ (asArrayOfPrimitives shape2) -- ineffective :(
asArrayOfPrimitives x = [ x ]

instance Eq Shape where
  (==) (Circle point1 radius1) (Circle point2 radius2) = (point1 == point2) && (radius1 == radius2)
  (==) (Rectangle point11 point12) (Rectangle point21 point22) = (point11 == point21) && (point12 == point22)
  (==) (Overlay shape11 shape12) (Overlay shape21 shape22) = (asArrayOfPrimitives (Overlay shape11 shape12)) == (asArrayOfPrimitives (Overlay shape21 shape22))
  (==) _ _ = False

-- Передвигает фигуру на x по горизонтали и на y по вертикали
-- Реализовать, используя то, что PointT -- моноид
slideShape :: Shape -> PointT -> Shape
slideShape (Circle point radius) shift = Circle (point <> shift) radius
slideShape (Rectangle point1 point2) shift = Rectangle (point1 <> shift) (point2 <> shift)
slideShape (Overlay shape1 shape2) shift = Overlay (slideShape shape1 shift) (slideShape shape2 shift)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround shape shifts = slideShape shape (mconcat shifts)

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.
instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) shape1 shape2 = Overlay shape1 shape2	-- But now we need to modify Shape.Eq so Overlay would become associative 

-- Is it unobvious how to implement an "empty" Shape. It can be solved by adding a fictional, special `Empty` Shape constructor, but it would be something weird
-- instance Monoid Shape where
--   mempty = undefined
