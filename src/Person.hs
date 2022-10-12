module Person where

import qualified Data.Set as Set

-- data Tree = ???

data Document = Passport (Int, Int) | CertOfBirth (String, Int)
                deriving (Show, Eq)
-- Тип данных для человека
data Person = Person
  { firstName :: String          -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: Maybe (Person, Person) -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq)

-- Создание ребенка данных родителей
createChild :: Person -> Person -> (String, String, Document) -> Person
createChild parent1 parent2 (fn, ln, cert) = Person { firstName = fn, lastName = ln, idNumber = Just cert, parents = Just (parent1, parent2), age = 0, formerLastNames = [] }

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них

greatestAncestor :: Person -> Person
greatestAncestor p =
  fst $ helper p
  where 
    helper :: Person -> (Person, Int)
    helper person = 
      case parents person of
        Nothing -> 
          (person, 0)
        Just (parent1, parent2) -> 
          let firstCandidate = helper parent1 in
          let secondCandidate = helper parent2 in
            if snd firstCandidate == snd secondCandidate
              then if (age $ fst firstCandidate) > (age $ fst secondCandidate)
                then (fst firstCandidate, snd firstCandidate + 1)
                else (fst secondCandidate, snd firstCandidate + 1)
            else if (snd firstCandidate > snd secondCandidate)
              then (fst firstCandidate, snd firstCandidate + 1)
              else (fst secondCandidate, snd firstCandidate + 1)

-- greatestAncestor person | parents person == Nothing = person
--                         | otherwise = person

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors = undefined

-- Возвращает семейное древо данного человека, описывающее его потомков.
-- descendants :: Person -> Set.Set Person -> Tree Person
-- descendants = undefined