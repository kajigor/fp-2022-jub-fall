module Person where

import qualified Data.Set as Set

data Tree a = Tree
  {
    curPerson :: a,
    descendantTrees :: Set.Set (Tree a)
  }
  deriving (Show, Eq, Ord)

data Document = Passport (Int, Int) | 
                BirthCert Int
                deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: Set.Set Person       -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: Set.Set Person -> String -> String -> Person
createChild parents firstName lastName = 
  Person {firstName = firstName, lastName = lastName, formerLastNames = [], 
  age = 0, idNumber = Nothing, parents = parents}

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = 
  let helpfunction person | parents person == Set.empty = (1, person)
                          | otherwise = helpfunction1 (Set.drop 1 (parents person)) (helpfunction (Set.elemAt 0 (parents person)))
                          where helpfunction1 parents (h, best_person) | parents == Set.empty = (h, best_person)
                                                                       | ((fst cur) > h || (fst cur) == h && ((age (snd cur)) > (age best_person))) = helpfunction1 (Set.drop 1 parents) cur
                                                                       | otherwise = helpfunction1 (Set.drop 1 parents) (h, best_person)
                                                                       where cur = helpfunction (Set.elemAt 0 parents)
  in snd(helpfunction person) 

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors h person | h == 0 = Set.singleton person
                   | otherwise = helpfunction (parents person)
                   where helpfunction parents | parents == Set.empty = Set.empty
                                              | otherwise = (Set.union (ancestors (h - 1) (Set.elemAt 0 parents)) (helpfunction (Set.drop 1 parents)))

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants person setPersons = Tree person (Set.map (\curperson -> descendants curperson setPersons) (Set.filter (\curperson -> Set.member person (parents curperson)) setPersons))