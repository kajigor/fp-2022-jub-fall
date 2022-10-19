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
slideShape (Circle center radius) add = Circle (center <> add) radius
slideShape (Rectangle p1 p2) add = Rectangle (p1 <> add) (p2 <> add)
slideShape (Overlay sh1 sh2) add = Overlay (slideShape sh1 add) (slideShape sh2 add)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround figure [] = figure
moveShapeAround figure (add : otherAdds) = moveShapeAround (slideShape figure add) otherAdds

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.

-- пересечение двух фигур -- ассоциативная операция
instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) sh1 sh2 = Overlay sh1 sh2

{-
Для моноида нужен нейтральный элемент, удовлетворяющий условиям:
   * mempty <> a = a
   * a <> mempty = a

Можно в качестве mempty взять всю плоскость
-}

minDouble = -9.223372036854776e18
maxDouble = 9.223372036854776e18

instance Monoid Shape where
  mempty = Rectangle (PointD minDouble maxDouble) (PointD maxDouble minDouble)
