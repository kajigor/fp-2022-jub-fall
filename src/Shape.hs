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
slideShape shape delta = case shape of
  Circle center radius -> Circle (center <> delta) radius
  Rectangle p1 p2 -> Rectangle (p1 <> delta) (p2 <> delta)
  Overlay s1 s2 -> Overlay (slideShape s1 delta) (slideShape s2 delta)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround shape ps = slideShape shape (mconcat ps)

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.

-- Если хочется ассоциативную операцию, то можно придумать например пересечение или объединение.
-- К сожалению для их реализации придётся по-хорошему искать выпуклую оболочку, что звучит больно, поэтому оставим как идею. Но это реально будет работать.

-- Альтернативно, можно просто составить Overlay shape1 shape2, но операция понятно не будет ассоциативной.
-- Можно было бы ввести Ord Shape и тогда просто хранить Data.Set вместо пары в Overlay. Тогда операция всё же будет ассоциативной.
instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) = undefined

-- Если использовать идею про пересечения, то можно использовать [-∞; +∞]^2 как нейтральный элемент.
-- Если про объединение фигур, то просто пустая фигура будет нейтральным элементом.
-- Если использовать идею про Overlay, то естесственного нейтрального элемента не придумать (пустое множество будет в семействе множеств).
instance Monoid Shape where
  mempty = undefined
