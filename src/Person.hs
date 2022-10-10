module Person where

import qualified Data.Set as Set

data Tree a = Leaf a | OneParent a (Tree a) | TwoParents a (Tree a) (Tree a) deriving (Show, Eq)

data Document = Passport( Int, Int) | BirthCert (String, Int) deriving (Show, Eq, Ord)



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
greatestAncestor :: Person -> Maybe Person
greatestAncestor person = fst (helper person) where
    helper :: Person -> (Maybe Person, Int)
    helper person =
        case parents person of
            (Nothing , Nothing) -> (Nothing, 0)
            (Nothing, Just p2) -> let (ancestor, depth) = helper p2
                                      in (ancestor, depth + 1)
            (Just p1, Nothing) -> let (ancestor, depth) = helper p1
                                      in (ancestor, depth + 1)
            (Just p1, Just p2) -> let (ancestor1, depth1) = helper p1
                                      in let (ancestor2, depth2) = helper p2
                                          in if depth1 > depth2
                                                 then (ancestor1, depth1 + 1)
                                             else (ancestor2, depth2 + 1)



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



-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants = undefined