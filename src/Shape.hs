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
slideShape (Circle a r) d = Circle (a <> d) r
slideShape (Rectangle a b) d = Rectangle (a <> d) (b <> d)
slideShape (Overlay a b) d = Overlay (slideShape a d) (slideShape b d)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround a d  = slideShape a (mconcat d)

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.

-- Если очень нужен моноид, можно взять в качестве операции конкатенацию списков фигур 
-- Если же использовать только уже реализованные функции, то есть Overlay в качестве операции 
-- и автоматически сгенерированный Eq в качестве равенства, то это даже не полугруппа,
-- потому что Overlay (Overlay x y) z != Overlay x (Overlay y z).
