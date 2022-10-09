module Person where

import qualified Data.Set as Set

data Tree = Orphan | ChildOf (Tree, Tree)

data Document = Document Int

-- Тип данных для человека
data Person = Person
  { firstName :: String           -- Имя, должно быть непустым
  , lastName :: String            -- Фамилия, должна быть непустой
  , formerLastNames :: [String]   -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                    -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document    -- Какое-то удостоверение личности
  , parents :: (Person, Person) -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq)

-- Создание ребенка данных родителей
createChild :: ??? Person -> Person
createChild = undefined

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor = undefined

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors = undefined

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants = undefined