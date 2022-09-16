module List where

-- [1, 2, 3, 4] === 1 : (2 : (3 : (4 : [])))

-- data [] a = [] | : a ([] a)
data List a = Nil -- пустой список без элементов, a.k.a. []
            | Cons a (List a) -- список, у которого есть голова типа a и хвост-список, a.k.a. :
            deriving (Show)

-- Считает сумму и произведение элементов списка целых чисел за один проход
-- Постарайтесь обобщить и использовать свертку, но это не обязательно
sumAndMult :: List Int -> (Int, Int)
sumAndMult Nil = (0, 1)
sumAndMult (Cons x xs) = let tailAns = sumAndMult xs in
    (x + fst tailAns, x * snd tailAns)

-- Найти максимальное значение в списке
-- Рекомендую использовать вспомогательную функцию, принимающую значение текущего максимума
maxNum :: List Int -> Int
maxNum Nil = minBound :: Int
maxNum (Cons x xs) = max x (maxNum xs)

-- Конкатенация двух списков, работает за длину первого списка
append :: List a -> List a -> List a
append Nil snd = snd
append (Cons x xs) snd = Cons x (append xs snd)

-- Всюду определенная функция взятия первого элемента
safeHead :: List a -> Maybe a
safeHead Nil = Nothing
safeHead (Cons x _) = Just x

-- Всюду определенная функция взятия хвоста списка
safeTail :: List a -> Maybe (List a)
safeTail Nil = Nothing
safeTail (Cons _ xs) = Just xs

-- Удваиваем каждый элемент списка
-- [1, 2] -> [2, 4]
double :: List Int -> List Int
double Nil = Nil
double (Cons x xs) = Cons (x * 2) (double xs)

-- Утраиваем каждый элемент списка
-- [1, 2] -> [3, 6]
triple :: List Int -> List Int
triple Nil = Nil
triple (Cons x xs) = Cons (x * 3) (triple xs)

-- Применяем функцию f к каждому элементу списка
map' :: (a -> b) -> List a -> List b
map' _ Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

-- Удвоение элементов списка через map
-- Используем (2*) как функцию удвоения числа
-- (2*) называется section of an infix operator
-- https://wiki.haskell.org/Section_of_an_infix_operator
double' :: List Int -> List Int
double' xs = map' (2*) xs

-- Можно использовать анонимную функцию, она же лямбда-функция
-- double' xs = map' (\x -> 2 * x) xs

-- Можно завести вспомогательную функцию f
-- double' xs = map' f xs
--   where f x = 2 * x

-- Утроение элементов списка через map
triple' :: List Int -> List Int
triple' xs = map' (*3) xs

-- Сложить все элементы списка целых чисел
sumListUp :: List Int -> Int
sumListUp (Cons x xs) = x + sumListUp xs
sumListUp Nil = 0

-- Перемножить элементы списка целых чисел
multListUp :: List Int -> Int
multListUp (Cons x xs) = x * multListUp xs
multListUp Nil = 1

-- -- Универсальная функция свертки
-- fold :: (Int -> Int -> Int) -> Int -> List Int -> Int
-- fold f acc (Cons x xs) = fold f (x `f` acc) xs
-- fold _ acc Nil = acc

-- Универсальная функция свертки
fold :: (a -> b -> b) -> b -> List a -> b
fold f acc (Cons x xs) = fold f (x `f` acc) xs
fold _ acc Nil = acc


-- Сложение элементов списка целых чисел
sumListUp' :: List Int -> Int
sumListUp' xs = fold (+) 0 xs

-- Перемножение элементов списка целых чисел
multListUp' :: List Int -> Int
multListUp' xs = fold (*) 1 xs
