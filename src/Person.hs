module Person where

import qualified Data.Set as Set

data Tree a = Node a (Set.Set (Tree a))
                deriving (Show, Eq, Ord)

data Document = Passport (Int, Int) | CertOfBirth (String, Int)
                deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String          -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: Maybe (Person, Person) -- (Биологические) родители данного человека. 
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: Person -> Person -> (String, String, Document) -> Person
createChild parent1 parent2 (fn, ln, doc@(CertOfBirth _)) = Person { firstName = fn, lastName = ln, idNumber = Just doc, parents = Just (parent1, parent2), age = 0, formerLastNames = [] }
createChild parent1 parent2 (fn, ln, _) = Person { firstName = fn, lastName = ln, idNumber = Nothing, parents = Just (parent1, parent2), age = 0, formerLastNames = [] } -- CertOfBirth wasn't provided

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
              then if age (fst firstCandidate) > age (fst secondCandidate)
                then (fst firstCandidate, snd firstCandidate + 1)
                else (fst secondCandidate, snd firstCandidate + 1)
            else if snd firstCandidate > snd secondCandidate
              then (fst firstCandidate, snd firstCandidate + 1)
              else (fst secondCandidate, snd firstCandidate + 1)

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors 0 p = Set.singleton p
ancestors n p = case parents p of
  Just (p1, p2) -> Set.union (ancestors (n - 1) p1) (ancestors (n - 1) p2)
  Nothing -> Set.empty

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants person people = Node person (Set.map (\p -> descendants p people) (children person)) where
  children :: Person -> Set.Set Person
  children p = Set.filter isChild people where
    isChild c = case parents c of 
      Nothing -> False
      Just (p1, p2) -> p1 == p || p2 == p