{-# LANGUAGE InstanceSigs #-}
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
integrateWithError method f a b err =
  case method of
    MiddleRectange -> approximation method rectangleList err
    LinearApproximation -> approximation method linApp err
    ParabolicApproximation -> approximation method paraboloicList err
  where
    rectangleList = fmap (integrateWithStepsCount MiddleRectange f a b) (infListNumbs 10)

    paraboloicList :: InfList Double
    paraboloicList = zipWithInf (\x y -> (4 * x - y) / 3)
      (tailInf linApp) linApp

    linApp = linearApproximationList f a b


linearApproximationSeq :: (Double -> Double) -> Double -> Double -> Int -> Double
linearApproximationSeq f a b 0 = integrateWithStepsCount LinearApproximation f a b 10
linearApproximationSeq f a b n =
  integrateLinAppWithPrevValue f a b (10 * (2 ^ n)) $ linearApproximationSeq f a b (n - 1)


data InfList a = a ::: (InfList a)

instance Functor InfList where
  fmap :: (a -> b) -> InfList a -> InfList b
  fmap f (x ::: xs) = f x ::: fmap f xs

tailInf :: InfList a -> InfList a
tailInf (_ ::: xs) = xs

zipWithInf :: (t1 -> t2 -> a) -> InfList t1 -> InfList t2 -> InfList a
zipWithInf f (x ::: xs) (y ::: ys) = f x y ::: zipWithInf f xs ys

infListNumbs :: Int -> InfList Int
infListNumbs n = n ::: infListNumbs (n + 1)


linearApproximationList :: (Double -> Double) -> Double -> Double -> InfList Double
linearApproximationList f a b = fmap (linearApproximationSeq f a b) (infListNumbs 0)


theta :: Method -> Double
theta ParabolicApproximation = 1 / 15
theta _ = 1 / 3


approximation :: Method -> InfList Double -> Double -> Double
approximation method l@(_ ::: xs) err = go xs l
  where
    go :: InfList Double -> InfList Double -> Double
    go (x ::: (_ ::: xs')) (y ::: ys) | abs(x - y) < theta method / 2 * err = x
                             | otherwise = go xs' ys