{-# OPTIONS_GHC -Wno-type-defaults #-}
module Shape where

-- Точка на плоскости
data PointT = PointD Double Double
            deriving (Show, Eq)
-- Фигуры
data Shape = Circle PointT Double    -- Круг характеризуется координатой центра и радиусом
           | Rectangle PointT PointT -- Прямоугольник со сторонами, параллельными координатным осям,
           deriving (Show, Eq)       -- характеризуется координатами двух углов

-- Создание круга с центром в нуле
makeCircleAtZero :: Double -> Shape
makeCircleAtZero = Circle (PointD 0 0)

-- Вычисление площади фигуры
area :: Shape -> Double
area (Circle _ radius) = pi * radius ^ 2
area (Rectangle (PointD x0 y0) (PointD x1 y1)) =
  abs (x1 - x0) * abs (y1 - y0)

-- Трансформирует прямоугольник таким образом,
-- чтобы его левый нижний угол был первым аргументом конструктора,
-- а правый верхний -- вторым.
normalizeRectangle :: Shape -> Shape
normalizeRectangle (Rectangle (PointD x0 y0) (PointD x1 y1)) =
  Rectangle (PointD (min x0 x1) (min y0 y1)) (PointD (max x0 x1) (max y0 y1))
normalizeRectangle x = x

-- Проверяет, является ли фигура корректной
-- У круга должен быть положительный радиус
-- Стороны прямоугольника должны иметь положительную длину
validateShape :: Shape -> Bool
validateShape (Circle _ r) = r > 0
validateShape (Rectangle (PointD x0 y0) (PointD x1 y1)) = x0 /= x1 && y0 /= y1

-- Считает периметр фигуры
perimeter :: Shape -> Double
perimeter (Circle _ radius) = 2 * pi * radius
perimeter (Rectangle (PointD x0 y0) (PointD x1 y1)) = 2 * (abs (x0 - x1) + abs (y0 - y1))

-- Проверяет, является ли фигура квадратом
isSquare :: Shape -> Bool
isSquare (Rectangle (PointD x0 y0) (PointD x1 y1)) =
  let x = abs (x0 - x1) in
  let y = abs (y0 - y1) in
  x == y
isSquare _ = False

-- Передвигает фигуру на x по горизонтали и на y по вертикали
slideShape :: Shape -> PointT -> Shape
slideShape shape point =
    case shape of
      Circle center radius -> Circle (slidePoint center point) radius
      Rectangle left right -> Rectangle (slidePoint left point) (slidePoint right point)
  where
    slidePoint (PointD x0 y0) (PointD x1 y1) =
      PointD (x0 + x1) (y0 + y1)

-- Проверяет, находится ли точка внутри данной фигуры
isPointInShape :: Shape -> PointT -> Bool
isPointInShape (Circle (PointD x0 y0) radius) (PointD x y) =
  (x - x0)^2 + (y - y0)^2 < radius^2
isPointInShape r (PointD x y) =
  let (Rectangle (PointD x0' y0') (PointD x1' y1')) = normalizeRectangle r in
  x0' < x && x < x1' && y0' < y && y < y1'