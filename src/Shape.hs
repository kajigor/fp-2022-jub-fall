{-# LANGUAGE InstanceSigs #-}
module Shape where
import Data.Function (on)

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
  (PointD x1 y1) <> (PointD x2 y2) = PointD (x1 + x2) (y1 + y2)

-- Фигуры
data Shape = Circle PointT Double    -- Круг характеризуется координатой центра и радиусом
           | Rectangle PointT PointT -- Прямоугольник со сторонами, параллельными координатным осям, характеризуется координатами двух углов
           | Overlay Shape Shape     -- Фигура, получающаяся наложением друг на друга двух других фигур
           deriving (Show, Eq)

-- Передвигает фигуру на x по горизонтали и на y по вертикали
-- Реализовать, используя то, что PointT -- моноид
slideShape :: Shape -> PointT -> Shape
slideShape (Circle center radius) shift = Circle (center <> shift) radius
slideShape (Rectangle p1 p2) shift = Rectangle (p1 <> shift) (p2 <> shift)
slideShape (Overlay s1 s2) shift = Overlay (slideShape s1 shift) (slideShape s2 shift)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround shape shifts = slideShape shape (mconcat shifts)

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.
instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) = max

instance Monoid Shape where
  mempty = Circle (PointD 0 0) 0

-- При таком упорядочивании ноль является минимальным числом.
compareByAbsThenSign :: Double -> Double -> Ordering
compareByAbsThenSign = (compare `on` abs) <> compare

-- Так как сначала числа сравниваются по абсолютному значению,
-- минимальная точка существует, и это PointD 0 0.
instance Ord PointT where
  compare (PointD x1 y1) (PointD x2 y2) = compareByAbsThenSign x1 x2 <> compareByAbsThenSign y1 y2

-- Сначала сравниваем по номеру конструктора, затем лексикографически сравниваем аргументы.
-- У Circle (PointD 0 0) 0 номер конструктора минимальный, а также минимальные аргументы
-- (так как мы сравниваем числа сначала по модулю, затем по знаку). Значит, это минимальный элемент Shape.
instance Ord Shape where
  -- Сначала по номеру конструктора
  compare Circle{} Rectangle{} = LT
  compare Circle{} Overlay{} = LT
  compare Rectangle{} Overlay{} = LT
  compare Overlay{} Rectangle{} = GT
  compare Overlay{} Circle{} = GT
  compare Rectangle{} Circle{} = GT
  -- Затем лексикографически по аргументам, если конструкторы совпали
  compare (Circle c1 r1) (Circle c2 r2) = compare c1 c2 <> compareByAbsThenSign r1 r2
  compare (Rectangle lt1 rb1) (Rectangle lt2 rb2) = compare lt1 lt2 <> compare rb1 rb2
  compare (Overlay first1 second1) (Overlay first2 second2) =
    compare first1 first2 <> compare second1 second2
