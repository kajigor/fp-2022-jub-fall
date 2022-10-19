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
data Shape
  = Empty
  | Circle PointT Double -- Круг характеризуется координатой центра и радиусом
  | Rectangle PointT PointT -- Прямоугольник со сторонами, параллельными координатным осям, характеризуется координатами двух углов
  | Overlay Shape Shape -- Фигура, получающаяся наложением друг на друга двух других фигур
  deriving (Show, Eq)

-- Передвигает фигуру на x по горизонтали и на y по вертикали
-- Реализовать, используя то, что PointT -- моноид
slideShape :: Shape -> PointT -> Shape
slideShape (Circle center r) vector = Circle (center <> vector) r
slideShape (Rectangle p1 p2) vector = Rectangle (p1 <> vector) (p2 <> vector)
slideShape (Overlay s1 s2) vector = Overlay (slideShape s1 vector) (slideShape s2 vector)
slideShape Empty _ = Empty

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround s points = slideShape s (mconcat points)

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.
-- Операция - наложение.
instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) s1 s2 = Overlay s1 s2

--  Нейтральный элемент - фигура с нулевой площадью. Тут два варианта - круг с 0 радиусом и квадрат с углами в одной точке. Но неприятно хранить еще их координаты, поэтому я завела eще фигуру - Empty.
instance Monoid Shape where
  mempty = Empty
