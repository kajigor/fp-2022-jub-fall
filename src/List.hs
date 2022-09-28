{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module List (filter', take', zipWith', squaresOfEvens, triangularNumbers) where

import Prelude hiding (foldl, foldr, reverse, map, zip)

-- Квадратичная реализация обращения списка.
-- Каждый раз, когда конкатенируете список к чему-то,
-- задумайтесь, может это можно переписать как-то иначе.
reverse :: [a] -> [a]
reverse [] = []
reverse (h : t) = reverse t ++ [h]

-- Линейная реализация обращения списка.
-- Аккумулятор накапливает элементы в правильном порядке, конкатенаций нет.
reverse' :: [a] -> [a]
reverse' xs =
    go [] xs
  where
    go acc [] = acc
    go acc (h : t) = go (h : acc) t

-- Правая свертка
-- foldr f acc [a1, a2, a3, ..., an] = a1 `f` (a2 `f` (a3 `f` ... (an `f` acc)...))
-- foldr (^) 4 [2,3] ->
  -- 2 ^ (foldr (^) 3 [4])  ->
    -- 2 ^ (3 ^ 4)
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ acc [] = acc
foldr f acc (x:xs) =
  x `f` foldr f acc xs

-- Левая свертка
-- foldl f acc [a1, a2, a3, ..., an] = (...((acc `f` a1) `f` a2) `f` ...) `f` an
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _ acc [] = acc
foldl f acc (x:xs) =
  foldl f (acc `f` x) xs

-- Превращает два списка в список кортежей из элементов списков на одинаковых позициях.
-- Если списки имеют разную длину, то длина результата -- минимальная из длин списков.
-- Работает на бесконечных списках.
zip :: [a] -> [b] -> [(a, b)]
zip (x:xs) (y:ys) = (x,y) : zip xs ys
zip _ _ = []

-- Проверяет списки целых чисел на равенство
eqList :: [Int] -> [Int] -> Bool
eqList [] [] = True
eqList (x:xs) (y:ys) | x == y = eqList xs ys -- guard
eqList _ _ = False

-- -- Оставляет только те элементы, на которых выполняется предикат p
-- -- filter isEven [1,2,3,4,5] -> [2,4]
-- filter :: (a -> Bool) -> [a] -> [a]
-- filter _ [] = []
-- filter p (h : t) =
--   if p h
--   then h : filter p t
--   else filter p t

-- -- Дальше несколько эквивалентных реализаций filter, демонстрирующих разный синтаксис
-- -- Альтернативная реализация фильтра с guard
-- filter :: (a -> Bool) -> [a] -> [a]
-- filter _ [] = []
-- filter p (h : t) | p h = h : filter p t
-- filter p (h : t) = filter p t

-- -- Альтернативная реализация фильтра с guard и otherwise
-- filter :: (a -> Bool) -> [a] -> [a]
-- filter _ [] = []
-- filter p (h : t) | p h = h : filter p t -- guard
--                  | otherwise = filter p t

-- -- Альтернативная реализация фильтра с case
-- filter :: (a -> Bool) -> [a] -> [a]
-- filter p xs =
--   case xs of
--     [] -> []
--     (h : t) | p h -> h : filter p t
--             | otherwise -> filter p t

-- Реализация map через foldr
map :: (a -> b) -> [a] -> [b]
map g xs =
    foldr f [] xs
  where
    f x acc = g x : acc

-- Берет первые n элементов списка.
-- Если в списке меньше n элементов -- возвращает все
take' :: Int -> [a] -> [a]
take' 0 xs = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs  


-- Реализуйте функцию filter с использованием foldr
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs =
    foldr f [] xs
  where
     f x acc | p x = x : acc
             | otherwise = acc

-- Функция-комбинация zip и map
-- Применяет функцию f к соответствующим элементам списков xs и ys
-- zipWith (+) [1,2,3] [10, 20, 30] = [11, 22, 33]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys
zipWith' f _ _ = []
-- Бесконечный список от a: [a..]
-- Диапазон от a до b: [a..b]
-- Диапазон от a до с c шагом (b - a): [a, b .. c]

-- Прямоугольные треугольники со сторонами не больше n
rightTriangles :: Int -> [(Int, Int, Int)]
rightTriangles n =
  [ (a, b, c) | a <- [1 .. n]
              , b <- [1 .. a] -- тут порядок важен
              , c <- [1 .. b] -- если переставить строку выше, то b окажется не объявленной
              , a * a == b * b + c * c ]

-- Квадраты четных целых чисел
-- В результате должен получиться бесконечный список
-- С помощью take из него можно взять конечный список
squaresOfEvens :: [Int]
squaresOfEvens = [x * x | x <- [0,2..]]

-- Бесконечный список из единиц
x :: [Int]
x = 1 : x

-- Бесконечный список, чередующий 0 и 1
y :: [Int]
y = 0 : 1 : y

-- Бесконечный список натуральных чисел
nat :: [Int]
nat = 1 : map (+1) nat

-- Бесконечный список чисел Фибоначчи
fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

-- Треугольные числа
-- https://en.wikipedia.org/wiki/Triangular_number
triangularNumbers :: [Int]
triangularNumbers = [n * (n - 1) `div` 2 | n <- [1..]]