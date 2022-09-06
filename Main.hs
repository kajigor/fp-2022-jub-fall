-- Точка на плоскости
data PointT = PointD Double Double
-- Фигуры
data Shape = Circle PointT Double    -- Круг характеризуется координатой центра и радиусом
           | Rectangle PointT PointT -- Прямоугольник со сторонами, параллельными координатным осям,
                                     -- характеризуется координатами двух углов

-- Создание круга с центром в нуле
makeCircleAtZero :: Double -> Shape
makeCircleAtZero r = Circle (PointD 0 0) r

-- Вычисление площади фигуры
area :: Shape -> Double
area (Circle center radius) = pi * radius ^ 2
area (Rectangle (PointD x0 y0) (PointD x1 y1)) =
  (abs (x1 - x0)) * (abs (y1 - y0))

-- Круг единичного радиуса
c :: Shape
c = Circle (PointD 0 0) 1

-- Квадрат с площадью 1
square :: Shape
square = Rectangle (PointD 0 0) (PointD 1 1)

-- Трансформирует прямоугольник таким образом,
-- чтобы его левый нижний угол был первым аргументом конструктора,
-- а правый верхний -- вторым.

-- Возможные ситуации:
-- 1: x0 > x1 && y0 > y1 => (x1 y1) (x0 y0)
-- 2: x0 < x1 && y0 > y1 => (x0 y1) (x1 y0)
-- 3: x0 > x1 && y0 < y1 => (x1 y0) (x0 y1)
-- 4: x0 < x1 && y0 < y1 => (x0 y0) (x1 y1) - точки выставлены верно

normalizeRectangle :: Shape -> Shape
normalizeRectangle (Rectangle (PointD x0 y0) (PointD x1 y1)) =
  if validateShape (Rectangle (PointD x0 y0) (PointD x1 y1))
    then
      if x0 > x1 && y0 > y1
        then Rectangle (PointD x1 y1) (PointD x0 y0)
        else
          if x0 < x1 && y0 > y1
            then Rectangle (PointD x0 y1) (PointD x1 y0)
            else
              if x0 > x1 && y0 < y1
                then Rectangle (PointD x1 y0) (PointD x0 y1)
                else Rectangle (PointD x0 y0) (PointD x1 y1)
    else Rectangle (PointD x0 y0) (PointD x1 y1)

-- другие фигуры передаем без изменений чтобы пройти тесты
normalizeRectangle anything = anything

-- Проверяет, является ли фигура корректной
-- У круга должен быть положительный радиус
-- Стороны прямоугольника должны иметь положительную длину
validateShape :: Shape -> Bool
validateShape (Circle center radius) = radius > 0
validateShape (Rectangle (PointD x0 y0) (PointD x1 y1)) = not (abs (x0 - x1) == 0 || abs (y0 - y1) == 0)

-- Считает периметр фигуры
perimeter :: Shape -> Double
perimeter (Circle center radius) = 2 * pi * radius
perimeter (Rectangle (PointD x0 y0) (PointD x1 y1)) = do
  let (Rectangle (PointD x0_n y0_n) (PointD x1_n y1_n)) = normalizeRectangle (Rectangle (PointD x0 y0) (PointD x1 y1))
  2 * (x1_n - x0_n) + 2 * (y1_n - y0_n)

-- Проверяет, является ли фигура квадратом
isSquare :: Shape -> Bool
isSquare (Circle center radius) = False
isSquare (Rectangle (PointD x0 y0) (PointD x1 y1)) = x1 - x0 == y1 - y0

-- Передвигает фигуру на x по горизонтали и на y по вертикали
slideShape :: Shape -> PointT -> Shape
slideShape (Circle (PointD x0 y0) r) (PointD x y) = Circle (PointD (x0 + x) (y0 + y)) r
slideShape (Rectangle (PointD x0 y0) (PointD x1 y1)) (PointD x y) =
  Rectangle (PointD (x0 + x) (y0 + y)) (PointD (x1 + x) (y1 + y))

-- Проверяет, находится ли точка внутри данной фигуры
-- НЕ включая границы
isPointInShape :: Shape -> PointT -> Bool
isPointInShape (Circle (PointD x0 y0) r) (PointD x y) = (x0 - x) ^ 2 + (y0 - y) ^ 2 < r ^ 2
isPointInShape (Rectangle (PointD x0 y0) (PointD x1 y1)) (PointD x y) = 
  validateShape(Rectangle (PointD x0 y0) (PointD x1 y1)) 
  && (do
    let (Rectangle (PointD x0_n y0_n) (PointD x1_n y1_n)) = normalizeRectangle (Rectangle (PointD x0 y0) (PointD x1 y1))
    (x > x0_n && x < x1_n) && (y > y0_n && y < y1_n)
  )

-- В результате выполнения программы в консоль должно напечататься True
-- Если решите не реализовывать одну из функций, закомментируйте соответствующий ей тест
main = do
  print $ and [ testValidateShape
              , testPerimeter
              , testNormalizeRectangle
              , testIsSquare
              , testSlideShape
              , testIsPointInShape
              ]

testValidateShape :: Bool
testValidateShape =
  and [ validateShape c1  `shouldBe` True
      , validateShape c2  `shouldBe` True
      , validateShape c3  `shouldBe` True
      , validateShape nc1 `shouldBe` False
      , validateShape nc2 `shouldBe` False
      , validateShape r1  `shouldBe` True
      , validateShape r2  `shouldBe` True
      , validateShape r3  `shouldBe` True
      , validateShape nr1 `shouldBe` False
      , validateShape nr2 `shouldBe` False
      , validateShape nr3 `shouldBe` False
      , validateShape sq1 `shouldBe` True
      , validateShape sq2 `shouldBe` True
      , validateShape sq3 `shouldBe` True
      , validateShape sq4 `shouldBe` True
      ]

testPerimeter =
  and [ perimeter c1  `shouldBeDouble` 0.6283185307179586
      , perimeter c2  `shouldBeDouble` 6.283185307179586
      , perimeter c3  `shouldBeDouble` 62.83185307179586
      , perimeter r1  `shouldBeDouble` 0.6000000000000001
      , perimeter r2  `shouldBeDouble` 4
      , perimeter r3  `shouldBeDouble` 1664
      , perimeter sq1 `shouldBeDouble` 8
      , perimeter sq2 `shouldBeDouble` 16
      , perimeter sq3 `shouldBeDouble` 16
      ]

testNormalizeRectangle :: Bool
testNormalizeRectangle =
  and [ normalizeRectangle c1  `shouldBeShape` Circle (PointD 0 0) 0.1
      , normalizeRectangle c2  `shouldBeShape` Circle (PointD 0 0) 1
      , normalizeRectangle c3  `shouldBeShape` Circle (PointD 0 0) 10
      , normalizeRectangle r1  `shouldBeShape` Rectangle (PointD 0 0) (PointD 0.1 0.2)
      , normalizeRectangle r2  `shouldBeShape` Rectangle (PointD (-1) 0) (PointD 0 1)
      , normalizeRectangle r3  `shouldBeShape` Rectangle (PointD (-777) (-42)) (PointD 13 0)
      , normalizeRectangle sq1 `shouldBeShape` Rectangle (PointD 0 0) (PointD 2 2)
      , normalizeRectangle sq2 `shouldBeShape` Rectangle (PointD (-2) 0) (PointD 2 4)
      , normalizeRectangle sq3 `shouldBeShape` Rectangle (PointD (-2) (-2)) (PointD 2 2)
      ]

testIsSquare :: Bool
testIsSquare =
  and [ isSquare c1  `shouldBe` False
      , isSquare c2  `shouldBe` False
      , isSquare c3  `shouldBe` False
      , isSquare r1  `shouldBe` False
      , isSquare r2  `shouldBe` True
      , isSquare r3  `shouldBe` False
      , isSquare sq1 `shouldBe` True
      , isSquare sq2 `shouldBe` True
      , isSquare sq3 `shouldBe` True
      ]

testSlideShape :: Bool
testSlideShape =
  and [ slideShape c1  p1 `shouldBeShape` Circle (PointD 0 0) 0.1
      , slideShape c2  p1 `shouldBeShape` Circle (PointD 0 0) 1
      , slideShape c3  p1 `shouldBeShape` Circle (PointD 0 0) 10
      , slideShape r1  p1 `shouldBeShape` Rectangle (PointD 0 0) (PointD 0.1 0.2)
      , slideShape r2  p1 `shouldBeShape` Rectangle (PointD (-1) 0) (PointD 0 1)
      , slideShape r3  p1 `shouldBeShape` Rectangle (PointD 13 (-42)) (PointD (-777) 0)
      , slideShape sq1 p1 `shouldBeShape` Rectangle (PointD 0 0) (PointD 2 2)
      , slideShape sq2 p1 `shouldBeShape` Rectangle (PointD (-2) 0) (PointD 2 4)
      , slideShape sq3 p1 `shouldBeShape` Rectangle (PointD (-2) (-2)) (PointD 2 2)
      , slideShape c1  p2 `shouldBeShape` Circle (PointD 1 0) 0.1
      , slideShape c2  p2 `shouldBeShape` Circle (PointD 1 0) 1
      , slideShape c3  p2 `shouldBeShape` Circle (PointD 1 0) 10
      , slideShape r1  p2 `shouldBeShape` Rectangle (PointD 1 0) (PointD 1.1 0.2)
      , slideShape r2  p2 `shouldBeShape` Rectangle (PointD 0 0) (PointD 1 1)
      , slideShape r3  p2 `shouldBeShape` Rectangle (PointD 14 (-42)) (PointD (-776) 0)
      , slideShape sq1 p2 `shouldBeShape` Rectangle (PointD 1 0) (PointD 3 2)
      , slideShape sq2 p2 `shouldBeShape` Rectangle (PointD (-1) 0) (PointD 3 4)
      , slideShape sq3 p2 `shouldBeShape` Rectangle (PointD (-1) (-2)) (PointD 3 2)
      , slideShape c1  p3 `shouldBeShape` Circle (PointD 0 1) 0.1
      , slideShape c2  p3 `shouldBeShape` Circle (PointD 0 1) 1
      , slideShape c3  p3 `shouldBeShape` Circle (PointD 0 1) 10
      , slideShape r1  p3 `shouldBeShape` Rectangle (PointD 0 1) (PointD 0.1 1.2)
      , slideShape r2  p3 `shouldBeShape` Rectangle (PointD (-1) 1) (PointD 0 2)
      , slideShape r3  p3 `shouldBeShape` Rectangle (PointD 13 (-41)) (PointD (-777) 1)
      , slideShape sq1 p3 `shouldBeShape` Rectangle (PointD 0 1) (PointD 2 3)
      , slideShape sq2 p3 `shouldBeShape` Rectangle (PointD (-2) 1) (PointD 2 5)
      , slideShape sq3 p3 `shouldBeShape` Rectangle (PointD (-2) (-1)) (PointD 2 3)
      , slideShape c1  p4 `shouldBeShape` Circle (PointD (-1) 1) 0.1
      , slideShape c2  p4 `shouldBeShape` Circle (PointD (-1) 1) 1
      , slideShape c3  p4 `shouldBeShape` Circle (PointD (-1) 1) 10
      , slideShape r1  p4 `shouldBeShape` Rectangle (PointD (-1) 1) (PointD (-0.9) 1.2)
      , slideShape r2  p4 `shouldBeShape` Rectangle (PointD (-2) 1) (PointD (-1) 2)
      , slideShape r3  p4 `shouldBeShape` Rectangle (PointD 12 (-41)) (PointD (-778) 1)
      , slideShape sq1 p4 `shouldBeShape` Rectangle (PointD (-1) 1) (PointD 1 3)
      , slideShape sq2 p4 `shouldBeShape` Rectangle (PointD (-3) 1) (PointD 1 5)
      , slideShape sq3 p4 `shouldBeShape` Rectangle (PointD (-3) (-1)) (PointD 1 3)
      , slideShape c1  p5 `shouldBeShape` Circle (PointD 1 (-1)) 0.1
      , slideShape c2  p5 `shouldBeShape` Circle (PointD 1 (-1)) 1
      , slideShape c3  p5 `shouldBeShape` Circle (PointD 1 (-1)) 10
      , slideShape r1  p5 `shouldBeShape` Rectangle (PointD 1 (-1)) (PointD 1.1 (-0.8))
      , slideShape r2  p5 `shouldBeShape` Rectangle (PointD 0 (-1)) (PointD 1 0)
      , slideShape r3  p5 `shouldBeShape` Rectangle (PointD 14 (-43)) (PointD (-776) (-1))
      , slideShape sq1 p5 `shouldBeShape` Rectangle (PointD 1 (-1)) (PointD 3 1)
      , slideShape sq2 p5 `shouldBeShape` Rectangle (PointD (-1) (-1)) (PointD 3 3)
      , slideShape sq3 p5 `shouldBeShape` Rectangle (PointD (-1) (-3)) (PointD 3 1)
      ]

testIsPointInShape :: Bool
testIsPointInShape =
  and [ isPointInShape c1  p1 `shouldBe` True
      , isPointInShape c1  p2 `shouldBe` False
      , isPointInShape c1  p3 `shouldBe` False
      , isPointInShape c1  p4 `shouldBe` False
      , isPointInShape c1  p5 `shouldBe` False
      , isPointInShape c2  p1 `shouldBe` True
      , isPointInShape c2  p2 `shouldBe` False
      , isPointInShape c2  p3 `shouldBe` False
      , isPointInShape c2  p4 `shouldBe` False
      , isPointInShape c2  p5 `shouldBe` False
      , isPointInShape c3  p1 `shouldBe` True
      , isPointInShape c3  p2 `shouldBe` True
      , isPointInShape c3  p3 `shouldBe` True
      , isPointInShape c3  p4 `shouldBe` True
      , isPointInShape c3  p5 `shouldBe` True
      , isPointInShape r1  p1 `shouldBe` False
      , isPointInShape r1  p2 `shouldBe` False
      , isPointInShape r1  p3 `shouldBe` False
      , isPointInShape r1  p4 `shouldBe` False
      , isPointInShape r1  p5 `shouldBe` False
      , isPointInShape r2  p1 `shouldBe` False
      , isPointInShape r2  p2 `shouldBe` False
      , isPointInShape r2  p3 `shouldBe` False
      , isPointInShape r2  p4 `shouldBe` False
      , isPointInShape r2  p5 `shouldBe` False
      , isPointInShape r3  p1 `shouldBe` False
      , isPointInShape r3  p2 `shouldBe` False
      , isPointInShape r3  p3 `shouldBe` False
      , isPointInShape r3  p4 `shouldBe` False
      , isPointInShape r3  p5 `shouldBe` True
      , isPointInShape sq1 p1 `shouldBe` False
      , isPointInShape sq1 p2 `shouldBe` False
      , isPointInShape sq1 p3 `shouldBe` False
      , isPointInShape sq1 p4 `shouldBe` False
      , isPointInShape sq1 p5 `shouldBe` False
      , isPointInShape sq2 p1 `shouldBe` False
      , isPointInShape sq2 p2 `shouldBe` False
      , isPointInShape sq2 p3 `shouldBe` True
      , isPointInShape sq2 p4 `shouldBe` True
      , isPointInShape sq2 p5 `shouldBe` False
      , isPointInShape sq3 p1 `shouldBe` True
      , isPointInShape sq3 p2 `shouldBe` True
      , isPointInShape sq3 p3 `shouldBe` True
      , isPointInShape sq3 p4 `shouldBe` True
      , isPointInShape sq3 p5 `shouldBe` True
      ]

c1 :: Shape
c1 = makeCircleAtZero 0.1

c2 :: Shape
c2 = makeCircleAtZero 1

c3 :: Shape
c3 = makeCircleAtZero 10

nc1 :: Shape
nc1 = makeCircleAtZero 0

nc2 :: Shape
nc2 = makeCircleAtZero (-2)

r1 :: Shape
r1 = Rectangle (PointD 0 0) (PointD 0.1 0.2)

r2 :: Shape
r2 = Rectangle (PointD (-1) 0) (PointD 0 1)

r3 :: Shape
r3 = Rectangle (PointD 13 (-42)) (PointD (-777) 0)

nr1 :: Shape
nr1 = Rectangle (PointD 0 0) (PointD 0 0)

nr2 :: Shape
nr2 = Rectangle (PointD 0 1) (PointD 0 2)

nr3 :: Shape
nr3 = Rectangle (PointD 0 1) (PointD 2 1)

sq1 :: Shape
sq1 = Rectangle (PointD 0 0) (PointD 2 2)

sq2 :: Shape
sq2 = Rectangle (PointD (-2) 0) (PointD 2 4)

sq3 :: Shape
sq3 = Rectangle (PointD (-2) (-2)) (PointD 2 2)

sq4 = Rectangle (PointD (-1) 1) (PointD 1 (-1))

p1 :: PointT
p1 = PointD 0 0

p2 :: PointT
p2 = PointD 1 0

p3 :: PointT
p3 = PointD 0 1

p4 :: PointT
p4 = PointD (-1) 1

p5 :: PointT
p5 = PointD 1 (-1)

eqPoint :: PointT -> PointT -> Bool
eqPoint (PointD x0 y0) (PointD x1 y1) = x0 == x1 && y0 == y1

eqShape (Circle c0 r0) (Circle c1 r1) = eqPoint c0 c1 && r0 == r1
eqShape (Rectangle a0 b0) (Rectangle a1 b1) = eqPoint a0 a1 && eqPoint b0 b1
eqShape _ _ = False

shouldBeShape :: Shape -> Shape -> Bool
shouldBeShape = eqShape

eqDouble :: (Ord a, Num a) => a -> a -> a -> Bool
eqDouble x y eps = abs (x - y) < eps

shouldBeDouble :: Double -> Double -> Bool
shouldBeDouble x y = eqDouble x y 0000001

shouldBe :: Eq a => a -> a -> Bool
shouldBe x y = x == y
