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
  (<>) = add
    where
      add (PointD x1 y1) (PointD x2 y2) = PointD (x1 + x2) (y1 + y2)

-- Фигуры
data Shape
  = Circle PointT Double -- Круг характеризуется координатой центра и радиусом
  | Rectangle PointT PointT -- Прямоугольник со сторонами, параллельными координатным осям, характеризуется координатами двух углов
  | Overlay Shape Shape -- Фигура, получающаяся наложением друг на друга двух других фигур
  deriving (Show, Eq)

-- Передвигает фигуру на x по горизонтали и на y по вертикали
-- Реализовать, используя то, что PointT -- моноид
slideShape :: Shape -> PointT -> Shape
slideShape (Circle p1 r) p2 = Circle (p1 <> p2) r
slideShape (Rectangle p1 p2) p3 = Rectangle (p1 <> p3) (p2 <> p3)
slideShape (Overlay sh1 sh2) p = slideShape sh1 p <> slideShape sh2 p

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround = foldl slideShape

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.
instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) = Overlay

instance Monoid Shape where
  mempty = Circle (PointD 0 0) 0
