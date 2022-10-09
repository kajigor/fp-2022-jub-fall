module Person where

import qualified Data.Set as Set

data Tree a = Leaf a | OneChild a (Tree a) | TwoChildren a (Tree a) (Tree a) deriving (Show, Eq)

data Document = Passport (Int, Int) | Bc (Int, String) deriving (Show, Eq)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: (Maybe Person, Maybe Person)       -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq)

-- Создание ребенка данных родителей
createChild :: (Maybe Person, Maybe Person) Person -> String -> Person
createChild (Just father, Just mother) name = Person name (lastName father) [] 0 Nothing (Just father, Just mother)
createChild (Just father, Nothing) name = Person name (lastName father) [] 0 Nothing (Just father, Nothing)
createChild (Nothing, Just mother) name = Person name (lastName mother) [] 0 Nothing (Nothing, Just mother)
createChild (Nothing, Nothing) name = Person name "Sirota" [] 0 Nothing Nothing

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor = undefined

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors = undefined

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Tree Person
descendants person = case parents person of
  (Nothing, Nothing) -> Leaf person
  (Just father, Nothing) -> OneChild person (descendants father)
  (Nothing, Just mother) -> OneChild person (descendants mother)
  (Just father, Just mother) -> TwoChildren person (descendants father) (descendants mother)