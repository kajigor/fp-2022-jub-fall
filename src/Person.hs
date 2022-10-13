module Person where

import qualified Data.Set as Set

data Tree a = Tree { node :: a, children :: [Tree a] } deriving (Show, Eq)

instance Ord a => Ord (Tree a) where
  tree1 <= tree2 = node tree1 <= node tree2

data Document = Passport { series :: Int, number :: Int } | BirthCertificate { series_number :: Int } deriving (Show, Eq)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: (Maybe Person, Maybe Person) -- Родители данного человека. Считаем, что родителей всегда 2, так говорили на уроках биологии. Причем некоторых может не быть в системе, поэтому Maybe.
  } deriving (Show, Eq)

instance Ord Person where
  person1 <= person2 = age person1 <= age person2

-- instance Eq Person where
--   person1 == person2 | idNumber person1 == Nothing && idNumber person2 == Nothing = 
--                                                 firstName person1 ++ lastName person1 == firstName person2 ++ lastName person2
--                      | idNumber person1 == Nothing = False
--                      | idNumber person2 == Nothing = False
--                      | otherwise = idNumber person1 == idNumber person2

-- Создание ребенка данных родителей
-- считаем, что основной кейс -- рождение человека, документов пока нет. Должны быть другие функции, меняющин ту или иную характеристику.
createChild :: String -> String -> Maybe Person -> Maybe Person -> Person
createChild firstName lastName firstParent secondParent =
  Person {
    firstName = firstName,
    lastName = lastName,
    formerLastNames = [],
    age = 0,
    idNumber = Nothing,
    parents = (firstParent, secondParent)
  }

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = snd (greatestAncestor' (0, person) (0, person)) where
  greatestAncestor' (x, person1) (y, person2) | parents person2 == (Nothing, Nothing) && x > y = (x, person1)
                                              | parents person2 == (Nothing, Nothing) && x < y = (y, person2)
                                              | parents person2 == (Nothing, Nothing) && age person1 > age person2 = (x, person1)
                                              | parents person2 == (Nothing, Nothing) = (y, person2)
  greatestAncestor' (x, person1) (y, Person { parents = (Just parent, Nothing) })
                                      = greatestAncestor' (x, person1) (y + 1, parent)
  greatestAncestor' (x, person1) (y, Person { parents = (Nothing, Just parent) })
                                      = greatestAncestor' (x, person1) (y + 1, parent)
  greatestAncestor' (x, person1) (y, Person { parents = (Just parent1, Just parent2) })
                                      = greatestAncestor' (greatestAncestor' (x, person1) (y + 1, parent1)) (y + 1, parent2)


-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors level person = ancestors' level (Just person) Set.empty where
  ancestors' _ Nothing result = result
  ancestors' 0 (Just person) result = Set.insert person result
  ancestors' level (Just person) result = ancestors' (level - 1) (fst (parents person)) (ancestors' (level - 1) (snd (parents person)) result)

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Set.Set Person -> Person -> Tree Person
descendants persons person = Tree {
                               node = person,
                               children = Set.elems (Set.map (descendants persons) (Set.filter (isChild person) persons))
                             }

sameAs :: Maybe Person -> Person -> Bool
sameAs Nothing _ = False
sameAs (Just p1) p2 = p1 == p2

isChild :: Person -> Person -> Bool
isChild parent Person { parents = (p1, p2) } | p1 `sameAs` parent || p2 `sameAs` parent = True
                                               | otherwise = False