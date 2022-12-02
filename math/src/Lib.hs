module Lib ( integrateWithStepsCount, 
             Method(MiddleRectange, LinearApproximation, ParabolicApproximation),
             integrateWithError, linearApproximationList
            )
  where

data Method = MiddleRectange | LinearApproximation | ParabolicApproximation

integrateWithStepsCount :: Method -> (Double -> Double) -> Double -> Double -> Int -> Double
integrateWithStepsCount MiddleRectange f a b steps =
  let h = (b - a) / fromIntegral steps in
  let get_x = \i -> a + h * fromIntegral i in
  let term = \i -> f ((get_x i + get_x (i + 1)) / 2) in
  h * sum (map term [0..(steps - 1)])
integrateWithStepsCount LinearApproximation f a b steps =
  let h = (b - a) / fromIntegral steps in
  let f_i = \i -> f (a + h * fromIntegral i) in
  let terms = map f_i [1..(steps - 1)] in
  h * ((f a + f b) / 2 + sum terms)
integrateWithStepsCount ParabolicApproximation f a b steps =
  (4 * integrateWithStepsCount LinearApproximation f a b steps - 
    integrateWithStepsCount LinearApproximation f a b (steps `div` 2)) / 3


integrateLinAppWithPrevValue :: (Double -> Double) -> Double -> Double -> Int -> Double -> Double
integrateLinAppWithPrevValue f a b steps prev =
  let h = (b - a) / fromIntegral steps in
  let get_x = \i -> a + h * i in
  let new_terms = map (\i -> f $ get_x $ 2 * fromIntegral i + 1) [0..(steps `div` 2 - 1)] in
  prev / 2 + h * sum new_terms


integrateWithError :: Method -> (Double -> Double) -> Double -> Double -> Double -> Double
integrateWithError MiddleRectange f a b err = 
  approximation MiddleRectange (map (integrateWithStepsCount MiddleRectange f a b) [10..]) err
integrateWithError LinearApproximation f a b err = 
  approximation LinearApproximation (linearApproximationList f a b) err
integrateWithError ParabolicApproximation f a b err = 
  approximation ParabolicApproximation paraboloicList err
  where
    paraboloicList = zipWith (\x y -> (4 * x - y) / 3) 
      (tail $ linearApproximationList f a b) $ linearApproximationList f a b


linearApproximationSeq :: (Double -> Double) -> Double -> Double -> Int -> Double
linearApproximationSeq f a b 0 = integrateWithStepsCount LinearApproximation f a b 10
linearApproximationSeq f a b n = 
  integrateLinAppWithPrevValue f a b (10 * (2 ^ n)) $ linearApproximationSeq f a b (n - 1)


linearApproximationList :: (Double -> Double) -> Double -> Double -> [Double]
linearApproximationList f a b = map (linearApproximationSeq f a b) [0..]


theta :: Method -> Double
theta ParabolicApproximation = 1 / 15
theta _ = 1 / 3

approximation :: Method -> [Double] -> Double -> Double
approximation method l@(_ : xs) err = go xs l
  where 
    go :: [Double] -> [Double] -> Double
    go (x : _ : xs') (y : ys) | abs(x - y) < theta method / 2 * err = x
                             | otherwise = go xs' ys 
    go _ _ = undefined
approximation _ _ _ = undefined