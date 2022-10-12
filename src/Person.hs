module Person where

import qualified Data.Set as Set
import Data.Maybe

data Tree v = Leaf v | Node v [Tree v] deriving (Show, Eq)

data Document = Passport (Int, Int) | BirthCertificate (String, Int) deriving(Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: (Maybe Person,  Maybe Person)   -- Пара родителей, с вариантомм отсутствия кого-то из них
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: Maybe Person -> Maybe Person -> String -> String -> Person
createChild Nothing Nothing firstName lastName = Person {
    firstName = firstName
  , lastName = lastName
  , formerLastNames  = []
  , age = 0
  , idNumber = Nothing
  , parents = (Nothing, Nothing)
}
createChild (Just person1) Nothing firstName lastName = Person {
    firstName = firstName
  , lastName = lastName
  , formerLastNames  = []
  , age = 0
  , idNumber = Nothing
  , parents = (Just person1, Nothing)
}
createChild Nothing (Just person2) firstName lastName = Person {
    firstName = firstName
  , lastName = lastName
  , formerLastNames  = []
  , age = 0
  , idNumber = Nothing
  , parents = (Nothing, Just person2)
}
createChild (Just person1) (Just person2) firstName lastName = Person {
    firstName = firstName
  , lastName = lastName
  , formerLastNames  = []
  , age = 0
  , idNumber = Nothing
  , parents = (Just person1, Just person2)
}
--
---- Самый далекий предок данного человека.
---- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
---- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = fst (up person)
  where
    up :: Person -> (Person, Int)
    up person
      | ((fst (parents person)) == Nothing && (snd (parents person)) == Nothing) = (person, 0)
      | ((fst (parents person)) == Nothing) = plus (up p2)
      | ((snd (parents person)) == Nothing) = plus (up p1)
      | otherwise =
        let (per1, d1) = up p1
          in let (per2, d2) = up p2
            in if (d1, age per1) > (d2, age per2)
              then (per1, d1 + 1)
              else (per2, d2 + 1)
      where
        p1 = fromJust (fst (parents person))
        p2 = fromJust (snd (parents person))
        plus :: (Person, Int) -> (Person, Int)
        plus (person, ind) = (person, ind + 1)

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors lvl person
  | lvl == 0 = Set.singleton person
  | lvl < 0 = Set.empty
  | otherwise = Set.union (up p1) (up p2)
    where
      p1 = (fst (parents person))
      p2 = (snd (parents person))
      up :: Maybe Person -> Set.Set Person
      up parent
        | (parent /= Nothing) = ancestors (lvl - 1) (fromJust parent)
        | otherwise = Set.empty

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants person arr = case children of
    [] -> Leaf person
    _ -> Node person (map buf children)
    where
      children :: [Person]
      children = Set.toList (Set.filter isChild arr)
      isChild :: Person -> Bool
      buf :: Person -> Tree Person
      buf human = descendants human arr
      isChild child = case parents child of
           (Just p1, Just p2) -> p1 == person || p2 == person
           (Nothing, Nothing) -> False
           (Just p1, Nothing) -> p1 == person
           (Nothing, Just p2) -> p2 == person

