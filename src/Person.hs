module Person where

import qualified Data.Set as Set
import Data.Maybe (fromJust, isNothing, isJust)

-- (1) дерево состоит из людей и их детей
-- (2) дети тоже деревья, которые состоят из (1)
data Tree a = Tree {
    person :: a,
    children :: Set.Set (Tree a)
  }
  deriving (Show, Eq, Ord)

data Document = Passport (Int, Int) -- Номер паспорта состоит из серии и номера, выдается в 14 лет.
  | BirthCertificate Int -- Номер свидетельства о рождении.
  deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , document :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: Maybe (Person,Person)  -- Родители данного человека. Выбрать подходящий контейнер.
  -- Nothing - у человека нет родителей
  -- Just(a,b) - у человека два родителя (a и b)
  -- Just(a,a) - у человека один родитель (a)
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: (Person, Person) -> Person -> Person
createChild (parent1, parent2) person = Person {
  firstName = firstName person,
  lastName = lastName person,
  formerLastNames = formerLastNames person,
  age = age person,
  document = document person,
  parents = Just (parent1, parent2)
}

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person =
  if isNothing (parents person)
    then person
    else
      let personParents = fromJust(parents person) in
      let a = greatestAncestor (fst personParents)
          b =  greatestAncestor (snd personParents)
      in if age a > age b then a else b

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors 0 person =
    if isNothing (parents person)
    then Set.empty
    else
      let personParents = fromJust(parents person) in
        Set.fromList [fst personParents, snd personParents]

ancestors level person =
  if isNothing (parents person)
    then Set.empty
    else
      let personParents = fromJust(parents person) in
      let f = ancestors (level - 1) (fst personParents)
          s = ancestors (level - 1) (snd personParents)
      in Set.union f s


-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants currentPerson allPersons =
  if allPersons == Set.empty
    then Tree currentPerson Set.empty
    else
      let chidren = Set.filter (isParent currentPerson) allPersons in -- найти все детей для этого человека
      let chidrenTrees = Set.map (`descendants` allPersons) chidren   -- для каждого из ребенка собрать дерево
      in Tree currentPerson chidrenTrees                              -- собрать финальное дерево для человека

-- Проверяет, что первый переданный человек является родителем второго.
isParent :: Person -> Person -> Bool
isParent parent currentPerson =
  isJust (parents currentPerson) && (
    let personParents = fromJust(parents currentPerson) in
      (parent == fst personParents) || ( parent == snd personParents))