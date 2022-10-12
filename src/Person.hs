module Person where

import qualified Data.Set as Set
import qualified Data.Foldable as Foldable

data Tree a = Tree a (Set.Set (Tree a))
  deriving (Show, Eq, Ord)

data Document = Passport Int Int | BirthCertificate String Int
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
createChild :: Set.Set Person -> String -> String -> Document -> Maybe Person
createChild _ [] _ _ = Nothing -- name cannot be empty
createChild _ _ [] _ = Nothing -- surname cannot be empty
createChild parents firstName lastName document =
  Just (Person { firstName = firstName, lastName = lastName, formerLastNames = [],
           age = 0, idNumber = Just document, parents = parents })

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Maybe Person
greatestAncestor person
  | fst (helper person) == 0 = Nothing
  | otherwise = Just (snd (helper person))
  where
    helper person
      | Set.null (parents person) = (0, person)
      | otherwise = Foldable.maximumBy (\ a b -> compare (fst a, age (snd a)) (fst b, age (snd b))) (Set.map (\ (a, b) -> (a + 1, b)) (Set.map helper (parents person)))

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors n person
  | n <= 0 = Set.empty
  | n > 0 = helper n person
  where
    helper n person
      | n == 0 = Set.singleton person
      | otherwise = Set.unions (Set.map (helper (n - 1)) (parents person))

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants person people = Tree person (Set.map (\ x -> descendants x people) (Set.filter (\ x -> Set.member person (parents x)) people))
