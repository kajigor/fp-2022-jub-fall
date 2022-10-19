{-# LANGUAGE InstanceSigs #-}
module Shape where
import Data.List

-- Точка на плоскости
data PointT = PointD Double Double
            deriving (Show, Eq, Ord)

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
  (<>) (PointD ax ay) (PointD bx by) = PointD (ax+bx) (ay+by)

-- Фигуры
data Shape = Circle PointT Double    -- Круг характеризуется координатой центра и радиусом
           | Rectangle PointT PointT -- Прямоугольник со сторонами, параллельными координатным осям, характеризуется координатами двух углов
           | Overlay Shape Shape     -- Фигура, получающаяся наложением друг на друга двух других фигур
           deriving (Show, Eq, Ord)

-- Передвигает фигуру на x по горизонтали и на y по вертикали
-- Реализовать, используя то, что PointT -- моноид
slideShape :: Shape -> PointT -> Shape
slideShape (Circle s r) p =  Circle (s <> p) r
slideShape (Rectangle pa pb) p = Rectangle (pa <> p) (pb <> p)
slideShape (Overlay a b) p =  Overlay (slideShape a p) (slideShape b p)

-- Второй аргумент задает последовательность сдвигов фигуры.
moveShapeAround :: Shape -> [PointT] -> Shape
moveShapeAround s xs = slideShape s (foldr (<>) mempty xs)

-- Является ли Shape полугруппой? А моноидом?
-- Реализовать инстансы, если является. Иначе -- обосновать.

-- Очень хочется сказать, что пересечение фигур -- это и есть наша операция в полугруппе,
-- но это не так в текущей реализации, поскольку у нее нет ассоциативности,
-- но мы можем ее добавить например вот так:
instance Semigroup Shape where
  (<>) :: Shape -> Shape -> Shape
  (<>) a b = res where
    go :: Shape -> [Shape] 
    go (Overlay x y) = (go x) ++ (go y)
    go x = [x]

    sorted = sort (go a ++ go b)
    
    res = foldr Overlay (head sorted) (tail sorted)
-- Но при такой реализации нам необходим Ord. 



instance Monoid Shape where
  mempty = undefined
-- Не является моноидом в текущей реализации, но можно добавить конструктор Null,
-- описать в Ord, что для любого a: Null < a. Тогда можем сделать Null нейтральным элементом, 
-- а при операциях с ним выкидывать его из списка после сортировки.
-- Но так как это изменит изначальный класс Shape, то я не уверен, что такое решение правильно.



-- main = do
--   let a = Circle (PointD 1 1) 2
--   let b = Circle (PointD 10 10) 2
--   let c = Circle (PointD 100 100) 2

--   print "result 'Overlay':"
--   print (Overlay a (Overlay b c) == Overlay (Overlay a b) c)

--   print "result '<>':"
--   print (a <> (b <> c) == (a <> b) <> c)
