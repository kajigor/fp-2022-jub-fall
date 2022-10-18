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
           deriving (Show, Eq)

-- Передвигает фигуру на x по горизонтали и на y по вертикали
-- Реализовать, используя то, что PointT -- моноид
slideShape :: Shape -> PointT -> Shape
slideShape shape move = case shape of
  Circle center raidus -> Circle (center <> move) raidus
  Rectangle left right -> Rectangle (left <> move) (right <> move)
  Overlay sh1 sh2 -> Overlay (slideShape sh1 move) (slideShape sh2 move)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround shape points = slideShape shape (mconcat points)

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.

-- Если рассматривать в качестве операции пересечение или объединение, то ассоциативность, конечно, будет
-- Проблема заключается в том, что пересечеение фигур так просто не реализовать, чего одни окружности стоят.
-- Объединение же будет требовать от нас также чего-то умного по типу всех точек пересечения и выпуклой оболочки.
-- У нас есть Overlay для объединения, но он не ассоциативный

-- Как вариант можно сделать моноид - немного переписать overly, чтобы хранить в нем просто set

instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) = undefined

-- Для перечисленных операций нейтральные элементы довольно просты: пустой круг для объединения, "бесконечный" квадрат и пустое мн-во для overlay с сетом
instance Monoid Shape where
  mempty = undefined
