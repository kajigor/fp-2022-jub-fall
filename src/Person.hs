module Person where

import qualified Data.Set as Set

data Tree = Tree {
  me :: Person,
  children :: Set.Set Tree
} deriving (Show, Eq, Ord)

data Document = Passport (Int, Int) | MiscDocument String deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String                     -- Имя, должно быть непустым
  , lastName :: String                      -- Фамилия, должна быть непустой
  , formerLastNames :: [String]             -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                              -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document              -- Какое-то удостоверение личности
  , parents :: (Maybe Person, Maybe Person) -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: (Person, Person) -> Person
createChild (mother, father) = Person "Ivan" (lastName father) [] 0 Nothing (Just mother, Just father)

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = fst $ greatestWithDepth person
  where 
    greatestWithDepth person = increaseDepth $ case parents person of
      (Just p1, Just p2) -> maxAncestor (greatestWithDepth p1) (greatestWithDepth p2)
      (Just p1, Nothing) -> greatestWithDepth p1
      (Nothing, Just p2) -> greatestWithDepth p2
      (Nothing, Nothing) -> (person, 0)
    maxAncestor (person1, x1) (person2, x2) = if (x1 > x2) || (x1 == x2 && (age person1) > (age person2)) then
      (person1, x1) else (person2, x2)
    increaseDepth (x, y) = (x, y + 1)

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors 0 person = Set.singleton person
ancestors n person = maybeAncestor (n - 1) (fst $ parents person) `Set.union` maybeAncestor (n - 1) (snd $ parents person)
  where
    maybeAncestor n person = case person of
      Nothing -> Set.empty
      Just p -> ancestors n p

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree
descendants person allDescendants = Tree person (Set.map (`descendants` allDescendants) $ filterMyChildren person allDescendants)
  where
    filterMyChildren person allDescendants = Set.filter (isChild person) allDescendants
    isChild person child = case parents child of
      (Just p1, Just p2) -> p1 == person || p2 == person
      (Just p1, Nothing) -> p1 == person
      (Nothing, Just p2) -> p2 == person
      (Nothing, Nothing) -> False
