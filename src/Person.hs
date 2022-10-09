module Person where

import qualified Data.Set as Set
import Data.Function
import Data.Foldable

-- Дерево хранит корневую вершину, а также детей корневой вершины в виде списка.
data Tree a = Tree a (Set.Set (Tree a))
  deriving (Show, Eq, Ord)

data Document = Passport Int Int | BirthCertificate Int Int
  deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: Set.Set Person       -- Родители данного человека. Выбрать подходящий контейнер.
  -- Используется контейнер Set, так как нам не важен порядок родителей
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: Set.Set Person -> String -> String -> Maybe Document -> Person
createChild parents firstName lastName document =
  Person { firstName = firstName
         , lastName = lastName
         , formerLastNames = []
         , age = 0
         , idNumber = document
         , parents = parents }

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor = fst . greatestAncestorAndLevel
  where
    greatestAncestorAndLevel :: Person -> (Person, Int)
    greatestAncestorAndLevel person | null $ parents person = (person, 0)
    greatestAncestorAndLevel person = do
      let parentResults = Set.map greatestAncestorAndLevel $ parents person
      let (p, level) = maximumBy (compare `on` (\(p, level) -> (level, age p))) parentResults
      (p, level + 1)

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors level person = ancestorsOfSet level (Set.singleton person)
  where
    ancestorsOfSet :: Int -> Set.Set Person -> Set.Set Person
    ancestorsOfSet 0 people = people
    ancestorsOfSet level people | level > 0 = do
      let parentsOfPeople = foldr Set.union Set.empty (Set.map parents people)
      ancestorsOfSet (level - 1) parentsOfPeople
    ancestorsOfSet _ _ = Set.empty

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants person allPeople = do
  let children = Set.filter (Set.member person . parents) allPeople
  Tree person (Set.map (`descendants` allPeople) children)
