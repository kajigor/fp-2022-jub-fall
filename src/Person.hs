module Person where

import qualified Data.Set as Set

data Tree = Node Person (Set.Set Tree) deriving (Show, Eq, Ord)

data Document = Passport ( Int, Int) | BirthCert (String, Int) deriving (Show, Eq, Ord)



-- Тип данных для человека
data Person = Person
  { firstName :: String                         -- Имя, должно быть непустым
  , lastName :: String                          -- Фамилия, должна быть непустой
  , formerLastNames :: [String]                 -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                                  -- Возраст, должен быть неотрицательным
  , idNumber :: Document                        -- Какое-то удостоверение личности
  , parents :: (Maybe Person, Maybe Person)     -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq, Ord)



-- Создание ребенка данных родителей
createChild :: String -> String -> (Maybe Person, Maybe Person) -> Person

createChild name surname (p1, p2) =
      Person { firstName = name
             , lastName = surname
             , formerLastNames = []
             , age = 0
             , idNumber = BirthCert("ao", 123456)
             , parents = (p1, p2)
             }



-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них

chooseMaxDepth :: (Person, Int) -> (Person, Int) -> (Person, Int)
chooseMaxDepth (person1, depth1) (person2, depth2) =
    if depth1 > depth2
        then (person1, depth1)
    else if depth1 == depth2 && age person1 >= age person2
        then (person1, depth1)
    else (person2, depth2)

greatestAncestor :: Person -> Person
greatestAncestor person = fst (helper person) where
    helper :: Person -> (Person, Int)
    helper arg =
        case parents arg of
            (Nothing , Nothing) -> (arg, 0)
            (Nothing, Just p2) -> let (ancestor, depth) = helper p2
                                      in (ancestor, depth + 1)
            (Just p1, Nothing) -> let (ancestor, depth) = helper p1
                                      in (ancestor, depth + 1)
            (Just p1, Just p2) -> let (ancestor, depth) = chooseMaxDepth (helper p1) (helper p2)
                                      in (ancestor, depth + 1)



-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors level person
    | level < 0 = Set.empty
    | level == 0 = Set.singleton person
    | otherwise = case parents person of
        (Nothing , Nothing) -> Set.empty
        (Nothing, Just p2) -> ancestors (level - 1) p2
        (Just p1, Nothing) -> ancestors (level - 1) p1
        (Just p1, Just p2) -> Set.union (ancestors (level - 1) p1) (ancestors (level - 1) p2)



-- проверяет, является ли person родителем child
isChild :: Person -> Person -> Bool
isChild person child = fst (parents child) == Just person || snd (parents child) == Just person

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Set.Set Person -> Person -> Tree
descendants people person =
    let children = Set.filter (isChild person) people in
        if (null children)
            then Node person Set.empty
        else Node person (Set.map (descendants people) children)
