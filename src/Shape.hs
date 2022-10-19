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
slideShape (Circle c r) shift = Circle (c <> shift) r
slideShape (Rectangle p1 p2) shift = Rectangle (p1 <> shift) (p2 <> shift)
slideShape (Overlay a b) shift = Overlay (slideShape a shift) (slideShape b shift)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround shape [] = shape
moveShapeAround shape (s0 : shifts) = moveShapeAround  (slideShape shape s0) shifts

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.
instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) shape0 shape1 = Overlay shape0 shape1

-- не получится придумать нейтральный элемент overlay, то есть у нас не сущесствует какой-то пустой фигуры, которую
-- мы бы  могли сделать mempty (mempty <> x = x). Вернее невозможно задать пустую фигуру с помощью описанного нами
-- конструктора Shape
instance Monoid Shape where
  mempty = undefined
