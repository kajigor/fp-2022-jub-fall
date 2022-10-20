{-# LANGUAGE ImplicitParams #-}

module Test.Shape where

import Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import Test.HUnit.Approx ((@?~))

import Shape

shouldBeShape :: Shape -> Shape -> Assertion
shouldBeShape x y =
    let ?epsilon = 0.000001 in
    go x y
  where
    go (Circle c0 r0) (Circle c1 r1) = do
      shouldBePoint c0 c1
      r0 @?~ r1
    go (Rectangle x0 y0) (Rectangle x1 y1) = do
      shouldBePoint x0 x1
      shouldBePoint y0 y1
    go (Overlay x0 x1) (Overlay y0 y1) = do
      shouldBeShape x0 y0
      shouldBeShape x1 y1
    go _ _ = fail "Shapes are not equal"
    shouldBePoint (PointD x0 y0) (PointD x1 y1) = do
      x0 @?~ x1
      y0 @?~ y1


unit_slideShape = do
  slideShape c1  p1 `shouldBeShape` Circle (PointD 0 0) 0.1
  slideShape c2  p1 `shouldBeShape` Circle (PointD 0 0) 1
  slideShape c3  p1 `shouldBeShape` Circle (PointD 0 0) 10
  slideShape r1  p1 `shouldBeShape` Rectangle (PointD 0 0) (PointD 0.1 0.2)
  slideShape r2  p1 `shouldBeShape` Rectangle (PointD (-1) 0) (PointD 0 1)
  slideShape r3  p1 `shouldBeShape` Rectangle (PointD 13 (-42)) (PointD (-777) 0)
  slideShape c1  p2 `shouldBeShape` Circle (PointD 1 0) 0.1
  slideShape c2  p2 `shouldBeShape` Circle (PointD 1 0) 1
  slideShape c3  p2 `shouldBeShape` Circle (PointD 1 0) 10
  slideShape r1  p2 `shouldBeShape` Rectangle (PointD 1 0) (PointD 1.1 0.2)
  slideShape r2  p2 `shouldBeShape` Rectangle (PointD 0 0) (PointD 1 1)
  slideShape r3  p2 `shouldBeShape` Rectangle (PointD 14 (-42)) (PointD (-776) 0)
  slideShape c1  p3 `shouldBeShape` Circle (PointD 0 1) 0.1
  slideShape c2  p3 `shouldBeShape` Circle (PointD 0 1) 1
  slideShape c3  p3 `shouldBeShape` Circle (PointD 0 1) 10
  slideShape r1  p3 `shouldBeShape` Rectangle (PointD 0 1) (PointD 0.1 1.2)
  slideShape r2  p3 `shouldBeShape` Rectangle (PointD (-1) 1) (PointD 0 2)
  slideShape r3  p3 `shouldBeShape` Rectangle (PointD 13 (-41)) (PointD (-777) 1)
  slideShape c1  p4 `shouldBeShape` Circle (PointD (-1) 1) 0.1
  slideShape c2  p4 `shouldBeShape` Circle (PointD (-1) 1) 1
  slideShape c3  p4 `shouldBeShape` Circle (PointD (-1) 1) 10
  slideShape r1  p4 `shouldBeShape` Rectangle (PointD (-1) 1) (PointD (-0.9) 1.2)
  slideShape r2  p4 `shouldBeShape` Rectangle (PointD (-2) 1) (PointD (-1) 2)
  slideShape r3  p4 `shouldBeShape` Rectangle (PointD 12 (-41)) (PointD (-778) 1)
  slideShape c1  p5 `shouldBeShape` Circle (PointD 1 (-1)) 0.1
  slideShape c2  p5 `shouldBeShape` Circle (PointD 1 (-1)) 1
  slideShape c3  p5 `shouldBeShape` Circle (PointD 1 (-1)) 10
  slideShape r1  p5 `shouldBeShape` Rectangle (PointD 1 (-1)) (PointD 1.1 (-0.8))
  slideShape r2  p5 `shouldBeShape` Rectangle (PointD 0 (-1)) (PointD 1 0)
  slideShape r3  p5 `shouldBeShape` Rectangle (PointD 14 (-43)) (PointD (-776) (-1))

  slideShape o1 p1 `shouldBeShape` Overlay (Circle (PointD 0 0) 0.1) (Circle (PointD 0 0) 1)
  slideShape o2 p1 `shouldBeShape` Overlay (Circle (PointD 0 0) 0.1) (Circle (PointD 0 0) 10)
  slideShape o3 p1 `shouldBeShape` Overlay (Rectangle (PointD 0 0) (PointD 0.1 0.2)) (Rectangle (PointD (-1) 0) (PointD 0 1))
  slideShape o4 p1 `shouldBeShape` Overlay (Circle (PointD 0 0) 0.1) (Rectangle (PointD 13.0 (-42)) (PointD (-777.0) 0))

  slideShape o5 p1 `shouldBeShape` Overlay (Overlay (Circle (PointD 0 0) 0.1) (Circle (PointD 0 0) 1)) (Overlay (Circle (PointD 0 0) 0.1) (Circle (PointD 0 0) 10))
  slideShape o1 p2 `shouldBeShape` Overlay (Circle (PointD 1 0) 0.1) (Circle (PointD 1 0) 1)
  slideShape o2 p2 `shouldBeShape` Overlay (Circle (PointD 1 0) 0.1) (Circle (PointD 1 0) 10)
  slideShape o3 p2 `shouldBeShape` Overlay (Rectangle (PointD 1 0) (PointD 1.1 0.2)) (Rectangle (PointD 0 0) (PointD 1 1))
  slideShape o4 p2 `shouldBeShape` Overlay (Circle (PointD 1 0) 0.1) (Rectangle (PointD 14 (-42)) (PointD (-776) 0))
  slideShape o5 p2 `shouldBeShape` Overlay (Overlay (Circle (PointD 1 0) 0.1) (Circle (PointD 1 0) 1)) (Overlay (Circle (PointD 1 0) 0.1) (Circle (PointD 1 0) 10))
  slideShape o1 p3 `shouldBeShape` Overlay (Circle (PointD 0 1) 0.1) (Circle (PointD 0 1) 1)
  slideShape o2 p3 `shouldBeShape` Overlay (Circle (PointD 0 1) 0.1) (Circle (PointD 0 1) 10)
  slideShape o3 p3 `shouldBeShape` Overlay (Rectangle (PointD 0 1) (PointD 0.1 1.2)) (Rectangle (PointD (-1) 1) (PointD 0 2))
  slideShape o4 p3 `shouldBeShape` Overlay (Circle (PointD 0 1) 0.1) (Rectangle (PointD 13.0 (-41)) (PointD (-777.0) 1))
  slideShape o5 p3 `shouldBeShape` Overlay (Overlay (Circle (PointD 0 1) 0.1) (Circle (PointD 0 1) 1)) (Overlay (Circle (PointD 0 1) 0.1) (Circle (PointD 0 1) 10))
  slideShape o1 p4 `shouldBeShape` Overlay (Circle (PointD (-1) 1) 0.1) (Circle (PointD (-1) 1) 1)
  slideShape o2 p4 `shouldBeShape` Overlay (Circle (PointD (-1) 1) 0.1) (Circle (PointD (-1) 1) 10)
  slideShape o3 p4 `shouldBeShape` Overlay (Rectangle (PointD (-1) 1) (PointD (-0.9) 1.2)) (Rectangle (PointD (-2) 1) (PointD (-1) 2))
  slideShape o4 p4 `shouldBeShape` Overlay (Circle (PointD (-1) 1) 0.1) (Rectangle (PointD 12 (-41)) (PointD (-778.0) 1))
  slideShape o5 p4 `shouldBeShape` Overlay (Overlay (Circle (PointD (-1) 1) 0.1) (Circle (PointD (-1) 1) 1)) (Overlay (Circle (PointD (-1) 1) 0.1) (Circle (PointD (-1) 1) 10))
  slideShape o1 p5 `shouldBeShape` Overlay (Circle (PointD 1 (-1)) 0.1) (Circle (PointD 1 (-1)) 1)
  slideShape o2 p5 `shouldBeShape` Overlay (Circle (PointD 1 (-1)) 0.1) (Circle (PointD 1 (-1)) 10)
  slideShape o3 p5 `shouldBeShape` Overlay (Rectangle (PointD 1 (-1)) (PointD 1.1 (-0.8))) (Rectangle (PointD 0 (-1)) (PointD 1 0))
  slideShape o4 p5 `shouldBeShape` Overlay (Circle (PointD 1 (-1)) 0.1) (Rectangle (PointD 14 (-43.0)) (PointD (-776) (-1)))
  slideShape o5 p5 `shouldBeShape` Overlay (Overlay (Circle (PointD 1 (-1)) 0.1) (Circle (PointD 1 (-1)) 1)) (Overlay (Circle (PointD 1 (-1)) 0.1) (Circle (PointD 1 (-1)) 10))


unit_moves = do
  moveShapeAround c1  [p1] `shouldBeShape` Circle (PointD 0 0) 0.1
  moveShapeAround c2  [p1] `shouldBeShape` Circle (PointD 0 0) 1
  moveShapeAround c3  [p1] `shouldBeShape` Circle (PointD 0 0) 10
  moveShapeAround r1  [p1] `shouldBeShape` Rectangle (PointD 0 0) (PointD 0.1 0.2)
  moveShapeAround r2  [p1] `shouldBeShape` Rectangle (PointD (-1) 0) (PointD 0 1)
  moveShapeAround r3  [p1] `shouldBeShape` Rectangle (PointD 13 (-42)) (PointD (-777) 0)
  moveShapeAround o1  [p1] `shouldBeShape` Overlay (Circle (PointD 0 0) 0.1) (Circle (PointD 0 0) 1)
  moveShapeAround o2  [p1] `shouldBeShape` Overlay (Circle (PointD 0 0) 0.1) (Circle (PointD 0 0) 10)
  moveShapeAround o3  [p1] `shouldBeShape` Overlay (Rectangle (PointD 0 0) (PointD 0.1 0.2)) (Rectangle (PointD (-1) 0) (PointD 0 1))
  moveShapeAround o4  [p1] `shouldBeShape` Overlay (Circle (PointD 0 0) 0.1) (Rectangle (PointD 13.0 (-42)) (PointD (-777.0) 0))

  moveShapeAround o5  [p1] `shouldBeShape` Overlay (Overlay (Circle (PointD 0 0) 0.1) (Circle (PointD 0 0) 1)) (Overlay (Circle (PointD 0 0) 0.1) (Circle (PointD 0 0) 10))
  moveShapeAround c1  [p1,p2,p3,p4,p5] `shouldBeShape` Circle (PointD 1 1) 0.1
  moveShapeAround c2  [p1,p2,p3,p4,p5] `shouldBeShape` Circle (PointD 1 1) 1
  moveShapeAround c3  [p1,p2,p3,p4,p5] `shouldBeShape` Circle (PointD 1 1) 10
  moveShapeAround r1  [p1,p2,p3,p4,p5] `shouldBeShape` Rectangle (PointD 1 1) (PointD 1.1 1.2)
  moveShapeAround r2  [p1,p2,p3,p4,p5] `shouldBeShape` Rectangle (PointD 0 1) (PointD 1 2)
  moveShapeAround r3  [p1,p2,p3,p4,p5] `shouldBeShape` Rectangle (PointD 14 (-41)) (PointD (-776) 1)
  moveShapeAround o1  [p1,p2,p3,p4,p5] `shouldBeShape` Overlay (Circle (PointD 1 1) 0.1) (Circle (PointD 1 1) 1)
  moveShapeAround o2  [p1,p2,p3,p4,p5] `shouldBeShape` Overlay (Circle (PointD 1 1) 0.1) (Circle (PointD 1 1) 10)
  moveShapeAround o3  [p1,p2,p3,p4,p5] `shouldBeShape` Overlay (Rectangle (PointD 1 1) (PointD 1.1 1.2)) (Rectangle (PointD 0 1) (PointD 1 2))
  moveShapeAround o4  [p1,p2,p3,p4,p5] `shouldBeShape` Overlay (Circle (PointD 1 1) 0.1) (Rectangle (PointD 14 (-41)) (PointD (-776) 1))
  moveShapeAround o5  [p1,p2,p3,p4,p5] `shouldBeShape` Overlay (Overlay (Circle (PointD 1 1) 0.1) (Circle (PointD 1 1) 1)) (Overlay (Circle (PointD 1 1) 0.1) (Circle (PointD 1 1) 10))

unit_semigroup = do
  ((c1 <> c2) <> c3) @?= (c1 <> (c2 <> c3))

  let allShapes = [c2, c3, r1, r2, r3, o1, o2, o3, o4] -- without c1 and o5

  ((foldl) (<>) c1 (allShapes ++ [o5])) @?= ((foldr) (<>) o5 (c1 : allShapes))


makeCircleAtZero :: Double -> Shape
makeCircleAtZero = Circle mempty

c1 :: Shape
c1 = makeCircleAtZero 0.1

c2 :: Shape
c2 = makeCircleAtZero 1

c3 :: Shape
c3 = makeCircleAtZero 10

r1 :: Shape
r1 = Rectangle (PointD 0 0) (PointD 0.1 0.2)

r2 :: Shape
r2 = Rectangle (PointD (-1) 0) (PointD 0 1)

r3 :: Shape
r3 = Rectangle (PointD 13 (-42)) (PointD (-777) 0)

o1 :: Shape
o1 = Overlay c1 c2

o2 :: Shape
o2 = Overlay c1 c3

o3 :: Shape
o3 = Overlay r1 r2

o4 :: Shape
o4 = Overlay c1 r3

o5 :: Shape
o5 = Overlay o1 o2

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
