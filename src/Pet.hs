module Pet where

import Data.Function (on)
import Data.List (sortBy)


-- Тип данных для представления результатов сравнения
-- data Ordering = LT | EQ | GT deriving Show

-- Функция compare класса Ord позволяет сравнивать значения:

-- Prelude> compare 0 0
-- EQ
-- Prelude> compare 1 2
-- LT
-- Prelude> compare 42 13
-- GT

-- Prelude> compare 'a' 'b'
-- LT
-- Prelude> compare 'a' 'a'
-- EQ

-- Порядки образуют моноид, что позволяет сравнивать более сложные структуры данных.

-- instance Semigroup Ordering where
--     LT <> _ = LT
--     EQ <> y = y
--     GT <> _ = GT

-- instance Monoid Ordering where
--     mempty = EQ

-- Списки (и строки), например, сравниваются лексикографически:

-- Prelude> compare "" ""
-- EQ
-- Prelude> compare "abc" "cba"
-- LT
-- Prelude> compare "abc" "a"
-- GT

-- Что, если хочется сравнивать как-то иначе?
-- Можно сначала преобразовать сравнимые значения к виду,
-- который оставляет только важные для сравнения аспекты, и дальше сравнивать при помощи compare.
-- Для этого можно использовать функцию on из Data.Function.
-- https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Function.html#v:on
-- on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
-- on binOp f x y = binOp (f x) (f y)

-- Сравнение строк только по значению их первой буквы
compareByFirstLetter :: String -> String -> Ordering
compareByFirstLetter = compare `on` head

-- Prelude> compareByFirstLetter "abc" "ade"
-- EQ
-- Prelude> compareByFirstLetter "ade" "abc"
-- EQ
-- Prelude> compareByFirstLetter "abc" "cba"
-- LT


-- Функции из a в b тоже образуют моноид, если b -- это моноид

-- instance Semigroup b => Semigroup (a -> b) where
--      f <> g = \x -> f x <> g x
--
-- instance Monoid b => Monoid (a -> b) where
--      mempty = \x -> mempty

-- Несколько разных подходов к сравнению можно скомбинировать, используя <>.
-- Например, если мы хотим сравнивать пары строк сначала по второй строке, а потом по первой:
comparePair :: (Ord a, Ord b) => (a, b) -> (a, b) -> Ordering
comparePair = (compare `on` snd) <> (compare `on` fst)

-- Prelude> comparePair ("a", "b") ("b", "a")
-- GT
-- Prelude> compare ("a", "b") ("b", "a")
-- LT

data Identification = Identification Int deriving (Show, Eq, Ord)

data Person = Person
  { firstName :: String              -- Имя, должно быть непустым
  , lastName :: String               -- Фамилия, должна быть непустой
  , identification :: Identification -- Любая форма идентификации человека
  }
  deriving (Show, Eq)

data Animal = Dog | Cat | Bunny | Tarantula deriving (Show, Eq, Ord)

data Pet = Pet
  { name :: String
  , owner :: Person
  , species :: Animal
  }
  deriving (Show, Eq)

-- Отсортируйте питомцев по их хозяевам:
-- * Хозяева упорядочиваются сначала по фамилии, затем по имени.
-- * В случае, если два человека -- тезки, упорядочиваем по номеру документа.
-- * Если у одного хозяина больше одного питомца, сортируйте их сначала по типу, потом по имени.
-- Сортировку стоит делать при помощи функции sortBy из Data.List
compOwner :: Person -> Person -> Ordering
compOwner = (compare `on` lastName) <> (compare `on` firstName) <> (compare `on` identification)

compPet :: Pet -> Pet -> Ordering
compPet = (compOwner `on` owner) <> (compare `on` species) <> (compare `on` name)

sortPets :: [Pet] -> [Pet]
sortPets = sortBy compPet

