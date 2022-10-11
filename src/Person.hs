{-# LANGUAGE FlexibleContexts #-}

module Person where

import qualified Data.Set as Set

data Tree a = Leaf a | Node a [Tree a] deriving (Show)

data Document = BirthCertificate Int | Passport (Int, Int) deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String, -- Имя, должно быть непустым
    lastName :: String, -- Фамилия, должна быть непустой
    formerLastNames :: [String], -- Предыдущие фамилии, если фамилия менялась
    age :: Int, -- Возраст, должен быть неотрицательным
    idNumber :: Maybe Document, -- Какое-то удостоверение личности
    parents :: [Person] -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Eq, Ord)

instance Show Person where
  show p = firstName p ++ lastName p

-- Создание ребенка данных родителей
createChild :: String -> String -> Int -> Person -> Person -> Person
createChild fn ln d p1 p2 = Person {firstName = fn, lastName = ln, formerLastNames = [], age = 0, idNumber = Just (BirthCertificate d), parents = [p1, p2]}

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> (Person, Int)
greatestAncestor per = grAnc per 0
  where
    grAnc :: Person -> Int -> (Person, Int)
    grAnc p acc
      | null (parents p) = (p, acc)
      | length (parents p) == 1 = grAnc (head (parents p)) (acc + 1)
      | length (parents p) == 2 = choose (grAnc (head (parents p)) (acc + 1)) (grAnc (parents p !! 1) (acc + 1))
      | otherwise = undefined
      where
        choose (p1, acc1) (p2, acc2)
          | acc1 > acc2 = (p1, acc1)
          | acc1 < acc2 = (p2, acc2)
          | age p1 > age p2 = (p1, acc1)
          | otherwise = (p2, acc2)

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors r p = anc 0 r p
  where
    anc :: Int -> Int -> Person -> Set.Set Person
    anc level reqlev person
      | level < reqlev && length (parents person) == 0 = Set.empty
      | level < reqlev && length (parents person) == 1 = anc (level + 1) reqlev (parents person !! 0)
      | level < reqlev && length (parents person) == 2 = Set.union (anc (level + 1) reqlev (parents person !! 0)) (anc (level + 1) reqlev (parents person !! 1))
      | level == reqlev = Set.insert person Set.empty
      | otherwise = undefined

-- Возвращает семейное древо данного человека, описывающее его потомков.

addChildren :: Person -> [Person] -> Tree Person
addChildren per pl = Node per (map Leaf pl)

getChildren :: Person -> Set.Set Person -> [Person]
getChildren p st = Set.toList (Set.filter (elem p . parents) st)

descendants :: Person -> Set.Set Person -> Tree Person
descendants p st
  | null (getChildren p st) = Leaf p
  | otherwise = Node p (map (`descendants` st) (getChildren p st))
