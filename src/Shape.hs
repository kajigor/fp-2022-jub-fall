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
slideShape (Circle center radius) shift = Circle (center <> shift) radius
slideShape (Rectangle first second) shift = Rectangle (first <> shift) (second <> shift)
slideShape (Overlay first second) shift = Overlay (slideShape first shift) (slideShape second shift)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround shape shifts = slideShape shape (mconcat shifts)

-- Является ли Shape полугруппой? А моноидом?
-- Так как сейчас нeт ассоциативности, потому что 
-- в зависимости от того в каком порядке мы будем комбинировать фигуры через Overlay, то получи разные результат: -
-- (Overlay (Overlay (Circle mempty 1) (Circle mempty 2)) (Circle mempty 3)) /=
-- (Overlay (Circle mempty 1) (Overlay (Circle mempty 2) (Circle mempty 3)))
-- это не полугруппа.
-- Тем не менее если бы и была ассоциативность, то нейтральный элемент не должен был бы менять фигуру, значит он пуст,
-- так как содержится в каждой фигуре, Nothing так же не определен.

checkEq :: Shape -> Shape -> Bool
checkEq a b = (a == b)

instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) = undefined

instance Monoid Shape where
  mempty = undefined
