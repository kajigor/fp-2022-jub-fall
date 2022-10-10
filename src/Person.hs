module Person where

import qualified Data.Set as Set

data Tree = Tree Person (Set.Set Tree) deriving (Show, Eq)

instance Ord Tree where
  (<=) (Tree a _) (Tree b _) = a <= b

data Document = Document Int deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String                       -- Имя, должно быть непустым
  , lastName :: String                        -- Фамилия, должна быть непустой
  , formerLastNames :: [String]               -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                                -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document                -- Какое-то удостоверение личности
  , parents :: (Maybe Person, Maybe Person)   -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Eq, Ord)

instance Show Person where
  show Person{firstName = name, lastName = surname} = name ++ " " ++ surname

-- Создание ребенка данных родителей (документы выдадут позже)
createChild :: Person -> Person -> String -> Person
createChild father mother name = Person {
  firstName = name,
  lastName = lastName father, 
  formerLastNames = [],
  age = 0,
  idNumber = Nothing,
  parents = (Just father, Just mother)
}

data PersonWithDepth = PersonWithDepth Person Int deriving (Show, Eq)

instance Ord PersonWithDepth where 
  (<=) (PersonWithDepth pa da) (PersonWithDepth pb db) = (da, age pa) <= (db, age pb)

greatestAncestorHelper :: Person -> PersonWithDepth
greatestAncestorHelper person = 
  let fs = maybe (PersonWithDepth person 0) greatestAncestorHelper (fst $ parents person)
      ms = maybe (PersonWithDepth person 0) greatestAncestorHelper (snd $ parents person) in
  let PersonWithDepth answer depth = max fs ms in
  PersonWithDepth answer (depth + 1)

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = ancestor where PersonWithDepth ancestor _ = greatestAncestorHelper person

-- -- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors 0 person = Set.fromList [person]
ancestors k person = 
  let fs = maybe Set.empty (ancestors (k - 1)) (fst $ parents person)
      ms = maybe Set.empty (ancestors (k - 1)) (snd $ parents person)
  in Set.union fs ms

isParent :: Person -> Person -> Bool
isParent first second = Just first `elem` [fst p, snd p]
  where p = parents second

-- -- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree
descendants person people =
  let children = Set.filter (isParent person) people in
    Tree person (Set.map buildTree children)
    where buildTree someone = descendants someone people