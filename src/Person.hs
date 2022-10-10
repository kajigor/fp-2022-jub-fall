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
  , parents :: (Maybe Person, Maybe Person)       -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: Maybe Person -> Maybe Person -> String -> String -> Person
createChild parent1 parent2 firstName lastName = 
  Person {firstName = firstName, lastName = lastName, formerLastNames = [], 
  age = 0, idNumber = Nothing, parents = (parent1, parent2)}

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них

parent_max :: (Int, Person) ->  (Int, Person) -> (Int, Person)
parent_max (h1, person1) (h2, person2) | h1 > h2 = (h1, person1)
                                       | otherwise = (h2, person2)

greatestAncestorPair :: Person -> (Int, Person)
greatestAncestorPair person = 
  let helpfunction :: Maybe Person -> (Int, Person)
      helpfunction Nothing = (0, person)
      helpfunction parent = ((fst p) + 1, (snd p)) where p = (greatestAncestorPair parent') where (Just parent') = parent
      fstAncestor = (helpfunction (fst(parents person)))
      sndAncestor = (helpfunction (snd(parents person)))
  in (parent_max fstAncestor sndAncestor)

greatestAncestor :: Person -> Person
greatestAncestor person = snd(greatestAncestorPair person)

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors h person | h == 0 = Set.singleton person
                   | otherwise = (Set.union (helpfunction (fst(parents person))) (helpfunction(snd(parents person))))
                   where helpfunction parent | parent == Nothing = Set.empty 
                                             | otherwise = (ancestors (h - 1) parent') where (Just parent') = parent

-- Возвращает семейное древо данного человека, описывающее его потомков.

checkParent :: Person -> Person -> Bool
checkParent person curPerson = ((Just person) == (fst (parents curPerson))) || ((Just person) == (snd (parents curPerson))) 

descendants :: Person -> Set.Set Person -> Tree Person
descendants person setPersons = Tree person (Set.map (\curperson -> descendants curperson setPersons) (Set.filter (\curperson -> (checkParent person curperson)) setPersons))