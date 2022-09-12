{-# LANGUAGE ImplicitParams #-}

module Test.Shape where

import Test.HUnit (Assertion, assertBool, (@?=))
import Test.HUnit.Approx ((@?~))

import Shape

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

sq4 :: Shape
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

unit_testValidateShape = do
  validateShape c1  @?= True
  validateShape c2  @?= True
  validateShape c3  @?= True
  validateShape nc1 @?= False
  validateShape nc2 @?= False
  validateShape r1  @?= True
  validateShape r2  @?= True
  validateShape r3  @?= True
  validateShape nr1 @?= False
  validateShape nr2 @?= False
  validateShape nr3 @?= False
  validateShape sq1 @?= True
  validateShape sq2 @?= True
  validateShape sq3 @?= True
  validateShape sq4 @?= True


unit_testPerimeter =
  let ?epsilon = 0.000001 in
  do
    perimeter c1  @?~ 0.6283185307179586
    perimeter c2  @?~ 6.283185307179586
    perimeter c3  @?~ 62.83185307179586
    perimeter r1  @?~ 0.6000000000000001
    perimeter r2  @?~ 4
    perimeter r3  @?~ 1664
    perimeter sq1 @?~ 8
    perimeter sq2 @?~ 16
    perimeter sq3 @?~ 16

shouldBeShape = (@?=)

unit_testNormalizeRectangle = do
  normalizeRectangle c1  `shouldBeShape` Circle (PointD 0 0) 0.1
  normalizeRectangle c2  `shouldBeShape` Circle (PointD 0 0) 1
  normalizeRectangle c3  `shouldBeShape` Circle (PointD 0 0) 10
  normalizeRectangle r1  `shouldBeShape` Rectangle (PointD 0 0) (PointD 0.1 0.2)
  normalizeRectangle r2  `shouldBeShape` Rectangle (PointD (-1) 0) (PointD 0 1)
  normalizeRectangle r3  `shouldBeShape` Rectangle (PointD (-777) (-42)) (PointD 13 0)
  normalizeRectangle sq1 `shouldBeShape` Rectangle (PointD 0 0) (PointD 2 2)
  normalizeRectangle sq2 `shouldBeShape` Rectangle (PointD (-2) 0) (PointD 2 4)
  normalizeRectangle sq3 `shouldBeShape` Rectangle (PointD (-2) (-2)) (PointD 2 2)

unit_testIsSquare = do
  isSquare c1  @?= False
  isSquare c2  @?= False
  isSquare c3  @?= False
  isSquare r1  @?= False
  isSquare r2  @?= True
  isSquare r3  @?= False
  isSquare sq1 @?= True
  isSquare sq2 @?= True
  isSquare sq3 @?= True

unit_testSlideShape = do
  slideShape c1  p1 `shouldBeShape` Circle (PointD 0 0) 0.1
  slideShape c2  p1 `shouldBeShape` Circle (PointD 0 0) 1
  slideShape c3  p1 `shouldBeShape` Circle (PointD 0 0) 10
  slideShape r1  p1 `shouldBeShape` Rectangle (PointD 0 0) (PointD 0.1 0.2)
  slideShape r2  p1 `shouldBeShape` Rectangle (PointD (-1) 0) (PointD 0 1)
  slideShape r3  p1 `shouldBeShape` Rectangle (PointD 13 (-42)) (PointD (-777) 0)
  slideShape sq1 p1 `shouldBeShape` Rectangle (PointD 0 0) (PointD 2 2)
  slideShape sq2 p1 `shouldBeShape` Rectangle (PointD (-2) 0) (PointD 2 4)
  slideShape sq3 p1 `shouldBeShape` Rectangle (PointD (-2) (-2)) (PointD 2 2)
  slideShape c1  p2 `shouldBeShape` Circle (PointD 1 0) 0.1
  slideShape c2  p2 `shouldBeShape` Circle (PointD 1 0) 1
  slideShape c3  p2 `shouldBeShape` Circle (PointD 1 0) 10
  slideShape r1  p2 `shouldBeShape` Rectangle (PointD 1 0) (PointD 1.1 0.2)
  slideShape r2  p2 `shouldBeShape` Rectangle (PointD 0 0) (PointD 1 1)
  slideShape r3  p2 `shouldBeShape` Rectangle (PointD 14 (-42)) (PointD (-776) 0)
  slideShape sq1 p2 `shouldBeShape` Rectangle (PointD 1 0) (PointD 3 2)
  slideShape sq2 p2 `shouldBeShape` Rectangle (PointD (-1) 0) (PointD 3 4)
  slideShape sq3 p2 `shouldBeShape` Rectangle (PointD (-1) (-2)) (PointD 3 2)
  slideShape c1  p3 `shouldBeShape` Circle (PointD 0 1) 0.1
  slideShape c2  p3 `shouldBeShape` Circle (PointD 0 1) 1
  slideShape c3  p3 `shouldBeShape` Circle (PointD 0 1) 10
  slideShape r1  p3 `shouldBeShape` Rectangle (PointD 0 1) (PointD 0.1 1.2)
  slideShape r2  p3 `shouldBeShape` Rectangle (PointD (-1) 1) (PointD 0 2)
  slideShape r3  p3 `shouldBeShape` Rectangle (PointD 13 (-41)) (PointD (-777) 1)
  slideShape sq1 p3 `shouldBeShape` Rectangle (PointD 0 1) (PointD 2 3)
  slideShape sq2 p3 `shouldBeShape` Rectangle (PointD (-2) 1) (PointD 2 5)
  slideShape sq3 p3 `shouldBeShape` Rectangle (PointD (-2) (-1)) (PointD 2 3)
  slideShape c1  p4 `shouldBeShape` Circle (PointD (-1) 1) 0.1
  slideShape c2  p4 `shouldBeShape` Circle (PointD (-1) 1) 1
  slideShape c3  p4 `shouldBeShape` Circle (PointD (-1) 1) 10
  slideShape r1  p4 `shouldBeShape` Rectangle (PointD (-1) 1) (PointD (-0.9) 1.2)
  slideShape r2  p4 `shouldBeShape` Rectangle (PointD (-2) 1) (PointD (-1) 2)
  slideShape r3  p4 `shouldBeShape` Rectangle (PointD 12 (-41)) (PointD (-778) 1)
  slideShape sq1 p4 `shouldBeShape` Rectangle (PointD (-1) 1) (PointD 1 3)
  slideShape sq2 p4 `shouldBeShape` Rectangle (PointD (-3) 1) (PointD 1 5)
  slideShape sq3 p4 `shouldBeShape` Rectangle (PointD (-3) (-1)) (PointD 1 3)
  slideShape c1  p5 `shouldBeShape` Circle (PointD 1 (-1)) 0.1
  slideShape c2  p5 `shouldBeShape` Circle (PointD 1 (-1)) 1
  slideShape c3  p5 `shouldBeShape` Circle (PointD 1 (-1)) 10
  slideShape r1  p5 `shouldBeShape` Rectangle (PointD 1 (-1)) (PointD 1.1 (-0.8))
  slideShape r2  p5 `shouldBeShape` Rectangle (PointD 0 (-1)) (PointD 1 0)
  slideShape r3  p5 `shouldBeShape` Rectangle (PointD 14 (-43)) (PointD (-776) (-1))
  slideShape sq1 p5 `shouldBeShape` Rectangle (PointD 1 (-1)) (PointD 3 1)
  slideShape sq2 p5 `shouldBeShape` Rectangle (PointD (-1) (-1)) (PointD 3 3)
  slideShape sq3 p5 `shouldBeShape` Rectangle (PointD (-1) (-3)) (PointD 3 1)


unit_testIsPointInShape = do
  isPointInShape c1  p1 @?= True
  isPointInShape c1  p2 @?= False
  isPointInShape c1  p3 @?= False
  isPointInShape c1  p4 @?= False
  isPointInShape c1  p5 @?= False
  isPointInShape c2  p1 @?= True
  isPointInShape c2  p2 @?= False
  isPointInShape c2  p3 @?= False
  isPointInShape c2  p4 @?= False
  isPointInShape c2  p5 @?= False
  isPointInShape c3  p1 @?= True
  isPointInShape c3  p2 @?= True
  isPointInShape c3  p3 @?= True
  isPointInShape c3  p4 @?= True
  isPointInShape c3  p5 @?= True
  isPointInShape r1  p1 @?= False
  isPointInShape r1  p2 @?= False
  isPointInShape r1  p3 @?= False
  isPointInShape r1  p4 @?= False
  isPointInShape r1  p5 @?= False
  isPointInShape r2  p1 @?= False
  isPointInShape r2  p2 @?= False
  isPointInShape r2  p3 @?= False
  isPointInShape r2  p4 @?= False
  isPointInShape r2  p5 @?= False
  isPointInShape r3  p1 @?= False
  isPointInShape r3  p2 @?= False
  isPointInShape r3  p3 @?= False
  isPointInShape r3  p4 @?= False
  isPointInShape r3  p5 @?= True
  isPointInShape sq1 p1 @?= False
  isPointInShape sq1 p2 @?= False
  isPointInShape sq1 p3 @?= False
  isPointInShape sq1 p4 @?= False
  isPointInShape sq1 p5 @?= False
  isPointInShape sq2 p1 @?= False
  isPointInShape sq2 p2 @?= False
  isPointInShape sq2 p3 @?= True
  isPointInShape sq2 p4 @?= True
  isPointInShape sq2 p5 @?= False
  isPointInShape sq3 p1 @?= True
  isPointInShape sq3 p2 @?= True
  isPointInShape sq3 p3 @?= True
  isPointInShape sq3 p4 @?= True
  isPointInShape sq3 p5 @?= True