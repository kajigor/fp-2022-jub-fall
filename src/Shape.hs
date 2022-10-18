{-# LANGUAGE InstanceSigs #-}
module Shape where

-- Точка на плоскости
data PointT = PointD Double Double
            deriving (Show, Eq, Ord) -- Ord чтобы сделать Shape ассоциативным по объединению

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
           deriving (Show, Eq, Ord)  -- Ord, чтобы сделать Shape ассоциативным по объединению 

-- Передвигает фигуру на x по горизонтали и на y по вертикали
-- Реализовать, используя то, что PointT -- моноид
slideShape :: Shape -> PointT -> Shape
slideShape (Overlay s1 s2) shift = Overlay (slideShape s1 shift) (slideShape s2 shift)
slideShape (Rectangle p1 p2) shift = Rectangle (p1 <> shift) (p2 <> shift)
slideShape (Circle p r) shift = Circle (p <> shift) r

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround shape shifts = slideShape shape (mconcat shifts)

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.

-- По поводу моноида:
--   Если операция -- наложение (как я понял -- объединение) двух фигур, то непонятно, что такое нейтральный элемент
--   Можно, конечно, закостылить нейтральный элемент какой-нибудь точкой, но тогда,
--     во-первых, он будет не единственный,
--     во-вторый, а с чего бы вдруг нам не хочется считать точки фигурами.
--   Так что мой вывод -- нет (no) (nein) (нi) (non).

-- По поводу полугруппы:
--   Вообще можно ввести порядок на Shape и пихать Shape в Overlay в возрастающем порядке, 
--   чтобы сделать операцию ассоциативной.

instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) s1 s2 | s1 < s2 = Overlay s1 s2
             | otherwise = Overlay s2 s1

instance Monoid Shape where
  mempty = undefined
