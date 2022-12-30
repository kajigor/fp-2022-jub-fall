module Integration where

import GHC.Float (int2Double)

withinWithIndex :: Double -> [(Int, Double)] -> Either String (Int, Double)
withinWithIndex eps (x : y : xs) | abs (snd x - snd y) < eps = Right y
                        | otherwise = withinWithIndex eps (y : xs)
withinWithIndex _ xs = Left "Couldn't achieve precesision"

average :: [Double] -> Double
average a = sum a / int2Double (length a)

nextMidPoint :: (Double -> Double) -> Double -> Double -> (Int,  Double) -> (Int,  Double)
nextMidPoint f a b (n, previous) = let dx = (b - a) / int2Double n in
                                    (3 * n, (previous
                                                    + (b - a) * average (map (\x -> f (a + int2Double x * dx + dx / 6)) [0 .. n - 1])
                                                    + (b - a) * average (map (\x -> f (a + int2Double x * dx + dx * 5 / 6)) [0 .. n - 1])) / 3)

startMidPoint :: (Double -> Double) -> Double -> Double -> (Int, Double)
startMidPoint f a b = (1, f ((a + b) / 2) * (b - a))

evalIntegralMidPoint :: (Double -> Double) -> Double -> Double -> Double -> Either String Double
evalIntegralMidPoint f a b eps = snd <$> evalIntegralMidPointWithIndex f a b eps

evalIntegralMidPointWithIndex :: (Double -> Double) -> Double -> Double -> Double -> Either String (Int, Double)
evalIntegralMidPointWithIndex f a b eps = withinWithIndex eps (iterate (nextMidPoint f a b) (startMidPoint f a b))

nextTrapezoid :: (Double -> Double) -> Double -> Double -> (Int,  Double) -> (Int,  Double)
nextTrapezoid f a b (n, previous) = (2 * n, (previous
                                            + (b - a) * average (map (\x -> f (a + (int2Double x + 0.5) * (b - a) / int2Double n)) [0 .. n - 1])) / 2)

startTrapezoid :: (Double -> Double) -> Double -> Double -> (Int, Double)
startTrapezoid f a b = (1, ((f a + f b) / 2) * (b - a))

evalIntegralTrapezoid :: (Double -> Double) -> Double -> Double -> Double -> Either String Double
evalIntegralTrapezoid f a b eps = snd <$> evalIntegralTrapezoidWithIndex f a b eps

evalIntegralTrapezoidWithIndex :: (Double -> Double) -> Double -> Double -> Double -> Either String (Int, Double)
evalIntegralTrapezoidWithIndex f a b eps = withinWithIndex eps (iterate (nextTrapezoid f a b) (startTrapezoid f a b))

nextSimpson :: (Double -> Double) -> Double -> Double -> (Int, Double, Double) -> (Int,  Double, Double)
nextSimpson f a b (n, trapezoid, trapezoidPrev) = (2 * n, snd (nextTrapezoid f a b (n, trapezoid)), trapezoid)

startSimpson :: (Double -> Double) -> Double -> Double -> (Int, Double, Double)
startSimpson f a b = (2, ((f a + 2 * f ((a + b) / 2) + f b) / 4) * (b - a), ((f a + f b) / 2) * (b - a))

evalIntegralSimpson :: (Double -> Double) -> Double -> Double -> Double -> Either String Double
evalIntegralSimpson f a b eps = snd <$> evalIntegralSimpsonWithIndex f a b eps

evalIntegralSimpsonWithIndex :: (Double -> Double) -> Double -> Double -> Double -> Either String (Int, Double)
evalIntegralSimpsonWithIndex f a b eps = withinWithIndex eps (map (\(n, trapezoid, trapezoidPrev) -> (n, (4 * trapezoid - trapezoidPrev) / 3)) (iterate (nextSimpson f a b) (startSimpson f a b)))
