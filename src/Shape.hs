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
  mempty = PointD 0.0 0.0

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
slideShape (Circle center r) point = Circle (center <> point) r
slideShape (Rectangle left right) point = Rectangle (left <> point) (right <> point)
slideShape (Overlay shape1 shape2) point = Overlay (slideShape shape1 point) (slideShape shape2 point)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround = foldl slideShape

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.

-- Полугруппа - множество с заданной на нём ассоциативной бинарной операцией.
-- Смотрим на доступные бинарные операции над Shape.
-- Здесь есть только Overlay Shape Shape, но данная операция не является ассоциативной
-- (не указано, что порядок наложения фигур неважен):

-- s1 <> (s2 <> s3) = Overlay s1 (Overlay s2 s3)
-- (s1 <> s2) <> s3 = Overlay (Overlay s1 s2) s3

-- Для моноида необходимо наличие нейтрального элемента, который бы удовлетворял
-- mempty <> a = a
-- a <> mempty = a
-- но в Shape у нас нет конструктора, который бы обозначал пустую фигуру,
-- к которой можно было бы добавлять другие с помощью наложения.

instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) = undefined

instance Monoid Shape where
  mempty = undefined
