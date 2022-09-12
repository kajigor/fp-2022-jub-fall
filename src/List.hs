module List ( sumAndMult
            , maxNum
            , append
            , safeHead
            , safeTail
            , double
            , triple
            , map'
            , double'
            , triple'
            , sumListUp
            , multListUp
            , sumListUp'
            , multListUp'
            , List(..)
            ) where
import Data.Foldable ( Foldable(foldl') )

-- [1, 2, 3, 4] === 1 : (2 : (3 : (4 : [])))

-- data [] a = [] | : a ([] a)dsa
data List a = Empty -- пустой список без элементов, a.k.a. []
            | AtLeastOne a (List a) -- список, у которого есть голова типа a и хвост-список, a.k.a. :
            deriving (Show)

-- Считает сумму и произведение элементов списка целых чисел за один проход
-- Постарайтесь обобщить и использовать свертку, но это не обязательно
sumAndMult :: List Int -> (Int, Int)
sumAndMult = foldl' fld (0, 1)
    where
        fld :: Num a => (a, a) -> a -> (a, a)
        fld (s, m) x = (s + x, m * x)

-- Найти максимальное значение в списке
-- Рекомендую использовать вспомогательную функцию, принимающую значение текущего максимума
maxNum :: List Int -> Int
maxNum = foldl' max 0

-- Конкатенация двух списков, работает за длину первого списка
append :: List a -> List a -> List a
append Empty rhs = rhs
append (AtLeastOne x xs) rhs = AtLeastOne x (append xs rhs)

-- Всюду определенная функция взятия первого элемента
safeHead :: List a -> Maybe a
safeHead Empty = Nothing
safeHead (AtLeastOne x _) = Just x

-- Всюду определенная функция взятия хвоста списка
safeTail :: List a -> Maybe (List a)
safeTail Empty = Nothing
safeTail (AtLeastOne _ xs) = Just xs

-- Удваиваем каждый элемент списка
-- [1, 2] -> [2, 4]
double :: List Int -> List Int
double Empty = Empty
double (AtLeastOne x xs) = AtLeastOne (x * 2) (double xs)

-- Утраиваем каждый элемент списка
-- [1, 2] -> [3, 6]
triple :: List Int -> List Int
triple Empty = Empty
triple (AtLeastOne x xs) = AtLeastOne (x * 3) (triple xs)

-- Применяем функцию f к каждому элементу списка
map' :: (a -> b) -> List a -> List b
map' _ Empty = Empty
map' f (AtLeastOne x xs) = AtLeastOne (f x) (map' f xs)

-- Удвоение элементов списка через map
-- Используем (2*) как функцию удвоения числа
-- (2*) называется section of an infix operator
-- https://wiki.haskell.org/Section_of_an_infix_operator
double' :: List Int -> List Int
double' = map' (2*)

-- Можно использовать анонимную функцию, она же лямбда-функция
-- double' xs = map' (\x -> 2 * x) xs

-- Можно завести вспомогательную функцию f
-- double' xs = map' f xs
--   where f x = 2 * x

-- Утроение элементов списка через map
triple' :: List Int -> List Int
triple' = map' (*3)

-- Сложить все элементы списка целых чисел
sumListUp :: List Int -> Int
sumListUp (AtLeastOne x xs) = x + sumListUp xs
sumListUp Empty = 0

-- Перемножить элементы списка целых чисел
multListUp :: List Int -> Int
multListUp (AtLeastOne x xs) = x * multListUp xs
multListUp Empty = 1

-- -- Универсальная функция свертки
-- fold :: (Int -> Int -> Int) -> Int -> List Int -> Int
-- fold f acc (AtLeastOne x xs) = fold f (x `f` acc) xs
-- fold _ acc Empty = acc

-- Универсальная функция свертки
-- fold :: (a -> b -> b) -> b -> List a -> b
-- fold f acc (AtLeastOne x xs) = fold f (x `f` acc) xs
-- fold _ acc Empty = acc

instance Functor List where
    fmap _ Empty = Empty
    fmap f (AtLeastOne a lst) = AtLeastOne (f a) (f <$> lst)

instance Foldable List where
    foldMap _ Empty = mempty
    foldMap f (AtLeastOne x xs) = f x <> foldMap f xs

-- Сложение элементов списка целых чисел
sumListUp' :: List Int -> Int
sumListUp' = foldl' (+) 0

-- Перемножение элементов списка целых чисел
multListUp' :: List Int -> Int
multListUp' = foldl' (*) 1
