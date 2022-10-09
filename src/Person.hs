{-# LANGUAGE InstanceSigs #-}
module Person where

import qualified Data.Set as Set

data Tree a = Leaf a | Node a [Tree a] deriving (Show, Eq)


data Document = Passport (Int, Int) | BirthCertificate (String, Int) deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String, -- Имя, должно быть непустым
    lastName :: String, -- Фамилия, должна быть непустой
    formerLastNames :: [String], -- Предыдущие фамилии, если фамилия менялась
    age :: Int, -- Возраст, должен быть неотрицательным
    idNumber :: Maybe Document, -- Какое-то удостоверение личности
    parents :: (Maybe Person, Maybe Person) -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: Maybe Person -> Maybe Person -> String -> Person
createChild (Just parent1) (Just parent2) name =
  Person
    { firstName = name,
      lastName = lastName parent1 ++ "—" ++ lastName parent2, -- inspired by Kharitontcev-Beglov
      formerLastNames = [],
      age = 0,
      idNumber = Nothing,
      parents = (Just parent1, Just parent2)
    }
createChild (Just parent1) Nothing name =
  Person
    { firstName = name,
      lastName = lastName parent1,
      formerLastNames = [],
      age = 0,
      idNumber = Nothing,
      parents = (Just parent1, Nothing)
    }
createChild Nothing (Just parent2) name =
  Person
    { firstName = name,
      lastName = lastName parent2,
      formerLastNames = [],
      age = 0,
      idNumber = Nothing,
      parents = (Nothing, Just parent2)
    }
createChild Nothing Nothing name =
  Person
    { firstName = name,
      lastName = "N/A",
      formerLastNames = [],
      age = 0,
      idNumber = Nothing,
      parents = (Nothing, Nothing)
    }

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = ancestor
  where
    helper :: Person -> (Person, Int) -- (самый далекий предок, его уровень иерархии)
    helper x = case parents x of
      (Nothing, Nothing) -> (x, 0) -- считаем, что сирота является предком самого себя
      (Just a, Nothing) -> let (a', aDepth) = helper a in (a', aDepth + 1)
      (Nothing, Just a) -> let (a', aDepth) = helper a in (a', aDepth + 1)
      (Just a, Just b) ->
        let (a', aDepth) = helper a
         in let (b', bDepth) = helper b
             in if (aDepth, age a') > (bDepth, age b')
                  then (a', aDepth + 1)
                  else (b', bDepth + 1)

    (ancestor, _) = helper person

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors level person
  | level < 0 = Set.empty
  | level == 0 = Set.singleton person
  | otherwise = foldTuple (parents person) (ancestors (level - 1))
  where
    foldTuple :: (Maybe Person, Maybe Person) -> (Person -> Set.Set Person) -> Set.Set Person
    foldTuple t f = case t of
      (Nothing, Nothing) -> Set.empty
      (Just a, Nothing) -> f a
      (Nothing, Just a) -> f a
      (Just a, Just b) -> Set.union (f a) (f b)

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants person allPersons = case childs of
  [] -> Leaf person
  _ -> Node person (map descendants' childs)
  where
    childs :: [Person]
    childs = Set.toList (Set.filter isChild allPersons)
    
    isChild :: Person -> Bool
    isChild p = case parents p of
      (Just p1, Just p2) -> p1 == person || p2 == person
      (Just p1, _) -> p1 == person
      (_, Just p2) -> p2 == person
      _ -> False

    descendants' :: Person -> Tree Person
    descendants' p = descendants p allPersons