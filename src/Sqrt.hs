module Sqrt (sqrt, relativeSqrt, generalSqrt) where

-- Этот пример из статьи https://www.researchgate.net/publication/2452204_Why_Functional_Programming_Matters

import Prelude hiding (sqrt)
-- Newton-Raphson Square Roots
-- a_{i+1} = (a_i + n/a_i)/2

-- -- Реализация на абстрактном псевдокоде
--   x = a0
--   y = a0 + 2 * eps
--   while (abs(x-y) >= eps) {
--     y = x
--     x = x + n / x
--   }
-- -- y == квадратному корню из n

-- -- Напишем из этого функцию
-- sqrt (n) =
--   x = a0
--   y = a0 + 2 * eps
--   while (abs(x-y) >= eps) {
--     y = x
--     x = x + n / x
--   }
--   return y

-- -- Что, если хотим другой eps? Откуда вообще n?
-- sqrt (a0, n, eps) = -- антипаттерн: смешение зон ответственности
--   x = a0
--   y = a0 + 2 * eps
--   while (abs(x-y) >= eps) {
--     y = x
--     x = x + n / x
--   }
--   return y

-- -- Что, если мы хотим изменить способ оценки того, насколько x и y близки?
-- sqrt (a0, n, eps, areClose) =
--   x = a0
--   y = a0 + 2 * eps
--   while (areClose (x, y, eps)) {
--     y = x
--     x = x + n / x
--   }
--   return y

-- Дальше идиоматический функциональный подход

-- a_{i+1} = (a_i + n/a_i)/2

-- Вычисление следующего приближения
next :: Double -> Double -> Double
next n x = (x + n/x)/2

-- Effect handling

-- iterate f x == [x, f x, f (f x), ...]

-- iterate (next n) a0

-- Находит первые два числа в последовательности, которые достаточно близки
within :: Double -> [Double] -> Double
within eps (x : y : xs) | abs (x - y) < eps = x
                        | otherwise = within eps (y : xs)
within _ xs = error $ "invalid argument: " ++ show xs


-- Для вычисления квадратного корня строим последовательность приближений,
-- находим первое, которое достаточно близко к следующему
sqrt :: Double -> Double -> Double -> Double
sqrt a0 eps n =
  within eps (iterate (next n) a0)

-- Другой способ оценить, насколько значения близки.
relative :: Double -> [Double] -> Double
relative eps (x : y : xs) | abs (x / y - 1) < eps = x
                          | otherwise = relative eps (y : xs)
relative _ xs = error $ "invalid argument: " ++ show xs

-- Вычисление квадратного корня с другим расстоянием
relativeSqrt :: Double -> Double -> Double -> Double
relativeSqrt a0 eps n =
  relative eps (iterate (next n) a0)

-- Очень общая функция.
generalSqrt :: (Double -> [Double] -> Double) -> Double -> Double -> Double -> Double
generalSqrt areClose a0 eps n =
  areClose eps (iterate (next n) a0)

-- sqrt         = generalSqrt within
-- relativeSqrt = generalSqrt relative