module Person where

import qualified Data.Set as Set

data Tree a = Leaf a | Node a (Tree a) (Tree a)
              deriving(Show, Eq)

data Document = (Int, Int) | (String, Int)
              deriving(Show, Eq)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: Maybe Set.Set Person       -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq)

-- Создание ребенка данных родителей
createChild :: Person -> Person -> String -> String -> Maybe Document -> Person
createChild p1 p2 firstName lastName idNumber = 
  Person { 
    firstName = firstName, 
    lastName = lastName, 
    formerLastNames = [],
    age = 0,
    idNumber = idNumber, 
    parents = Set.Set [p1, p2] 
    }

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor = undefined

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors lvl person = helper (lvl) (person)
  where helper | 0 persons = persons
               | lvl person = helper (lvl - 1) (parents person)
               | lvl (h : t) = helper (lvl - 1) (parents h) ++ helper (lvl) t

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants = undefined