module Person where

import qualified Data.Set as Set


data Tree = Tree 
  { person :: Person
  , children :: [Tree]
  }
  deriving (Show, Eq, Ord)

data Document = Passport { series :: Int, number :: Int }
  deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: (Maybe Person, Maybe Person) -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: (Maybe Person, Maybe Person) -> Person
createChild (Just person1, Just person2) = Person "Vasya" (lastName person1) [] 0 Nothing (Just person1, Just person2)
createChild (Just person1, Nothing) = Person "Vasya" (lastName person1) [] 0 Nothing (Just person1, Nothing)
createChild (Nothing, Just person2) = Person "Vasya" (lastName person2) [] 0 Nothing (Nothing, Just person2)
createChild (Nothing, Nothing) = Person "Vasya" "Pupkin" [] 0 Nothing (Nothing, Nothing)

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = fst $ greatestWithAcc 0 (person, 0) person 
  where 
    greatestWithAcc depth (gr_person, x) person = case parents person of 
      (Just person1, Just person2) -> greatestWithAcc (depth + 1) (greatestWithAcc (depth + 1) (gr_person, x) person1) person2
      (Just person1, Nothing) -> greatestWithAcc (depth + 1) (gr_person, x) person1
      (Nothing, Just person2) -> greatestWithAcc (depth + 1) (gr_person, x) person2
      (Nothing, Nothing) -> greatestFromTwo (gr_person, x) (person, depth)

    greatestFromTwo (person1, x1) (person2, x2) = if (x1 > x2) || (x1 == x2 && (age person1) > (age person2)) then
      (person1, x1) else (person2, x2)
    

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors 0 person = Set.singleton person
ancestors depth person = case parents person of
  (Just person1, Just person2) -> Set.union (ancestors (depth - 1) person1) (ancestors (depth - 1) person2)
  (Just person1, Nothing) -> ancestors (depth - 1) person1
  (Nothing, Just person2) -> ancestors (depth - 1) person2
  (Nothing, Nothing) -> Set.empty

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree
descendants person children | children == Set.empty = Tree person []
                            | otherwise = Tree person $ map (\child -> descendants child otherDescendants) $ Set.toList directChildren
  where
    isChildOf parent child = (fst $ parents child) == Just parent || (snd $ parents child) == Just parent
    
    directChildren = Set.filter (isChildOf person) children

    otherDescendants = Set.difference children directChildren
    