module Syntax where

-- Эквивалентные реализации функций на списках.
-- Просто демонстрация, как еще можно писать.

sumList :: [Int] -> Int
-- sumList [] = 0
-- sumList (h : t) = h + sumList t

-- foldr :: (a -> b -> b) -> (b -> ([a] -> b))
-- (+) :: Int -> Int -> Int
-- 0 :: Int
-- foldr (+) :: Int -> [Int] -> Int
--              acc -> xs -> result
-- foldr (+) 0 :: [Int] -> Int
sumList = foldr (+) 0

sum2Lists :: [Int] -> [Int] -> [Int]
-- sum2Lists (x : xs) (y : ys) = x + y : sum2Lists xs ys
-- sum2Lists _ _ = []

-- uncurry ::  (a -> b -> c) -> (a, b) -> c
-- uncurry ::  (a -> (b -> c)) -> ((a, b) -> c)
-- sum2List xs ys = map (uncurry (+)) $ zip xs ys
sum2Lists = zipWith (+)

-- curry :: ((a, b) -> c) -> a -> b -> c


squareEven :: [Int]
-- squareEven = map (^2) (map (*2) [0..])
-- squareEven = map (\x -> (x * 2) ^ 2) [0..]
-- squareEven = map ((^2) . (*2)) [0..]
squareEven = map2funs (^2) (*2) [0..]

map2funs :: (b -> c) -> (a -> b) -> [a] -> [c]
map2funs f g = map (f . g)

-- (f . g) x = f (g x)

powEven :: Int -> [Int]
powEven n = map (^n) (map (*2) [0..])