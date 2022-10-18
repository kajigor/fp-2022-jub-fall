{-# LANGUAGE InstanceSigs #-}

module Shape where

-- Точка на плоскости
data PointT =
  PointD Double Double
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
data Shape
  = Circle PointT Double -- Круг характеризуется координатой центра и радиусом
  | Rectangle PointT PointT -- Прямоугольник со сторонами, параллельными координатным осям, характеризуется координатами двух углов
  | Overlay Shape Shape -- Фигура, получающаяся наложением друг на друга двух других фигур
  deriving (Show, Eq)

-- Передвигает фигуру на x по горизонтали и на y по вертикали
-- Реализовать, используя то, что PointT -- моноид
slideShape :: Shape -> PointT -> Shape
slideShape (Circle p r) delta = Circle (p <> delta) r
slideShape (Rectangle p1 p2) delta = Rectangle (p1 <> delta) (p2 <> delta)
slideShape (Overlay s1 s2) delta =
  Overlay (slideShape s1 delta) (slideShape s2 delta)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround s points = slideShape s (mconcat points)

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.
-- a <> b — Минимальный прямоугольник, в который можно вписать обе фигуры, если сдвинуть их центр в центр координат
instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) shape1 shape2 = Rectangle minPoint maxPoint
    where
      (minPoint, maxPoint) = minMaxCoordinates (Overlay shape1 shape2)
      -- min and max coordinates of a shape with slided center
      minMaxCoordinates :: Shape -> (PointT, PointT)
      minMaxCoordinates (Circle _ r) = (PointD (-r) (-r), PointD r r)
      minMaxCoordinates (Rectangle (PointD x1 y1) (PointD x2 y2)) =
        ( PointD (abs (x2 - x1) / (-2)) (abs (y2 - y1) / (-2))
        , PointD (abs (x2 - x1) / 2) (abs (y2 - y1) / 2))
      minMaxCoordinates (Overlay s1 s2) =
        ( PointD (min x1min x2min) (min y1min y2min)
        , PointD (max x1max x2max) (max y1max y2max))
        where
          (PointD x1min y1min, PointD x1max y1max) = minMaxCoordinates s1
          (PointD x2min y2min, PointD x2max y2max) = minMaxCoordinates s2

-- Круг с радиусом 0 можно вписать куда угодно, поэтому он нейтрален относительно операции, определенной выше
instance Monoid Shape where
  mempty = Circle (PointD 0 0) 0
