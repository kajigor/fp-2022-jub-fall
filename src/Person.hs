module Person where

import qualified Data.Set as Set
import Data.Maybe

data Tree = Vertex Person (Set.Set Tree) 
  deriving(Show, Eq, Ord)

data Document = Passport (Int, Int) | BirthCertificate (String, Int) 
  deriving(Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , document :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: (Maybe Person, Maybe Person) -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
-- Документ ребенок получит потом
createChild :: String -> String -> (Maybe Person, Maybe Person) -> Person
createChild firstName lastName (parent1, parent2) = 
  if (isNothing parent1 && isJust parent2)
  then createChild firstName lastName (parent2, parent1)
  else Person firstName lastName [] 0 Nothing (parent1, parent2)

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = snd (greatestAncestorWithLevel person)
  where
    greatestAncestorWithLevel :: Person -> (Int, Person)
    greatestAncestorWithLevel person 
      | (isJust parent1 && isJust parent2) =
        let ancestor1 = inc (greatestAncestorWithLevel (fromJust parent1)) in
        let ancestor2 = inc (greatestAncestorWithLevel (fromJust parent2)) in
        
        if (fst ancestor1 /= fst ancestor2) then max ancestor1 ancestor2
        else
          if (age (snd ancestor1) > age (snd ancestor2)) then ancestor1
          else ancestor2
      | otherwise =
        if (isJust parent1) then inc (greatestAncestorWithLevel (fromJust parent1))
        else (0, person)
      where
        parent1 = fst (parents person)
        parent2 = snd (parents person)
        inc :: (Int, Person) -> (Int, Person)
        inc (level, person) = (level + 1, person)

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors level person
  | (level == 0) = Set.singleton person
  | otherwise = Set.union (previous parent1) (previous parent2)
  where
    parent1 = fst (parents person)
    parent2 = snd (parents person)
    previous :: Maybe Person -> Set.Set Person
    previous parent
      | (isJust parent) = ancestors (level - 1) (fromJust parent)
      | otherwise = Set.empty

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree
descendants person people =
  let children = Set.filter isChild people in
  if (null children) then Vertex person Set.empty
  else Vertex person (Set.map trees children)
  where
    isChild :: Person -> Bool
    isChild candidate = 
      (Just person == fst (parents candidate)) || (Just person == snd (parents candidate))
    trees :: Person -> Tree 
    trees child = descendants child people
