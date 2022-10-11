{-# LANGUAGE InstanceSigs #-}

module Person where

import qualified Data.Set as Set
import Data.List

data Tree a = Tree a (Set.Set (Tree Person))
  deriving (Show, Eq, Ord)

data Document = Passport (Int, Int)    -- формат (xxxx, xxxxxx)
              | BirthCeritificate Int  -- формат xxxxxxxxxx
    deriving (Show, Eq, Ord)           --        <---10--->

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: Set.Set Person   -- Родители данного человека. Set хорош потому что может быть и empty, и размера 1, и больше.
  }
  deriving (Show, Eq)

-- СПОЙЛЕР: не читайте следующие три строчки, если вы сейчас едите
instance Ord Person where       
  (<=) :: Person -> Person -> Bool
  (<=) p1 p2 = (age p1) <= (age p2) 
                             
-- Создание ребенка данных родителей
createChild :: String -> String -> Set.Set Person -> Person
createChild firstName lastName parents = Person { firstName=firstName, lastName=lastName, formerLastNames=[], age=0, idNumber=Nothing, parents=parents }   

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor p = snd (getMaxAncestorWithLevel p)
  where 
    getMaxAncestorWithLevel :: Person -> (Int, Person)
    getMaxAncestorWithLevel person | (parents person) == Set.empty = (0, person)
                                   | otherwise = Set.findMax (Set.map (\pers -> getMaxAncestorWithLevel pers) (parents person))

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors level person | level < 0 = Set.empty
                       | level == 0 = Set.singleton person
                       | otherwise = Set.fold Set.union Set.empty (Set.map (ancestors (level-1)) (parents person))

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants person set = Tree person ((Set.map (\p -> descendants p set) (Set.filter (\p -> elem person (parents p)) set)))





