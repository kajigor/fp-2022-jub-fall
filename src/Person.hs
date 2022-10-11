module Person where

import qualified Data.Set as Set

data Tree a = Tree 
  { person :: Person
  , parent1 :: Maybe (Tree Person)
  , parent2 :: Maybe (Tree Person)
  , children :: [Person]
  }
  deriving (Show, Eq, Ord)

data Document = BirthCertificate (String, Int) | Passport (Int, Int) 
  deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , document :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: (Maybe Person, Maybe Person)       -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: String -> String -> String -> Int -> (Maybe Person, Maybe Person) -> Person
createChild firstName lastName series number (p1, p2) =
  Person firstName lastName [] 0 (Just (BirthCertificate (series, number))) (p1,p2)

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = result
  where
    greatestPersonWithDepth :: Person -> (Person, Int) 
    greatestPersonWithDepth person1 = case parents person1 of
      (Nothing, Nothing) -> (person1, 0)
      (Just person2, Just person3) ->
        let (pesron4, person2Depth) = greatestPersonWithDepth person2
         in let (person5, person3Depth) = greatestPersonWithDepth person3
             in if (person2Depth, age pesron4) > (person3Depth, age person5)
                  then (pesron4, person2Depth + 1)
                  else (person5, person3Depth + 1)
      (_, Just person2) -> let (pesron4,person2Depth) = greatestPersonWithDepth person2 in (pesron4, person2Depth + 1)
      (Just person2, _) -> let (pesron4, person2Depth) = greatestPersonWithDepth person2 in (pesron4, person2Depth + 1)
    (result, _) = greatestPersonWithDepth person

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors 0 person = Set.fromList [person]
ancestors n person = 
  let f = maybe Set.empty (ancestors (n - 1)) (fst (parents person))
      m = maybe Set.empty (ancestors (n - 1)) (snd (parents person))
  in Set.union f m

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants person list = parentTree person (Set.toList (Set.filter (isChild person) list))
  where
    parentTree :: Person -> [Person] -> Tree Person
    parentTree person list = case parents person of
      (Just person1, Just person2) -> Tree person (Just ( parentTree person1 [person])) (Just ( parentTree person2 [person])) list
      (Just person1, Nothing) -> Tree person (Just (parentTree person1 [person])) Nothing list
      (Nothing, Just person2) -> Tree person Nothing (Just (parentTree person2 [person])) list
      (Nothing, Nothing) -> Tree person Nothing Nothing list

    isChild parent child = fst (parents child) == Just parent || snd  (parents child) == Just parent