{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
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

pattern EmptyShape :: Shape
pattern EmptyShape <- Circle _ 0

-- Передвигает фигуру на x по горизонтали и на y по вертикали
-- Реализовать, используя то, что PointT -- моноид
slideShape :: Shape -> PointT -> Shape
slideShape (Circle c r) p = Circle (c <> p) r
slideShape (Rectangle p1 p2) p = Rectangle (p1 <> p) (p2 <> p)
slideShape (Overlay s1 s2) p = Overlay (slideShape s1 p) (slideShape s2 p)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround s l = slideShape s (mconcat l)

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.
instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) s1 s2 = Overlay s1 s2

instance Monoid Shape where
  -- mempty can be any shape that matches EmptyShape: Circle _ 0
  mempty :: Shape
  mempty = Circle (PointD 0 0) 0

