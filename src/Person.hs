{-# LANGUAGE FlexibleContexts #-}

module Person where

import qualified Data.Set as Set

data Tree a = Leaf a | Node a [Tree a] deriving (Eq, Show)

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
  show p = firstName p ++ " " ++ lastName p

-- Создание ребенка данных родителей
createChild :: String -> String -> Int -> Person -> Person -> Person
createChild fn ln d p1 p2 = Person {firstName = fn, lastName = ln, formerLastNames = [], age = 0, idNumber = Just (BirthCertificate d), parents = [p1, p2]}

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor per = fst (grAnc per 0)
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
ancestors = anc 0
  where
    anc :: Int -> Int -> Person -> Set.Set Person
    anc level reqlev person
      | level < reqlev && null (parents person) = Set.empty
      | level < reqlev && length (parents person) == 1 = anc (level + 1) reqlev (head (parents person))
      | level < reqlev && length (parents person) == 2 = Set.union (anc (level + 1) reqlev (head (parents person))) (anc (level + 1) reqlev (parents person !! 1))
      | level == reqlev = Set.insert person Set.empty
      | otherwise = undefined

-- Возвращает семейное древо данного человека, описывающее его потомков.

--addChildren :: Person -> [Person] -> Tree Person
--addChildren per pl = Node per (map Leaf pl)

getChildren :: Person -> Set.Set Person -> [Person]
getChildren p st = Set.toList (Set.filter (elem p . parents) st)

descendants :: Person -> Set.Set Person -> Tree Person
descendants p st
  | null (getChildren p st) = Leaf p
  | otherwise = Node p (map (`descendants` st) (getChildren p st))

-- TEST SECTION --
-- can't set up Test.Tasty.HUnit (Assertion, (@?=), assertBool) for some reasons --

p1 = Person {firstName = "name_1", lastName = "lastname_1", formerLastNames = [], age = 100, parents = [], idNumber = Just (Passport (1111, 111111))}

p2 = Person {firstName = "name_2", lastName = "lastname_2", formerLastNames = [], age = 105, parents = [], idNumber = Just (Passport (1111, 111111))}

p3 = Person {firstName = "name_3", lastName = "lastname_3", formerLastNames = [], age = 0, parents = [p1, p2], idNumber = Just (Passport (1111, 111111))}

p4 = Person {firstName = "name_4", lastName = "lastname_4", formerLastNames = [], age = 0, parents = [p1, p2], idNumber = Just (Passport (1111, 111111))}

p5 = Person {firstName = "name_5", lastName = "lastname_5", formerLastNames = [], age = 0, parents = [p1, p2], idNumber = Just (Passport (1111, 111111))}

p6 = Person {firstName = "name_6", lastName = "lastname_6", formerLastNames = [], age = 107, parents = [], idNumber = Just (Passport (1111, 111111))}

p7 = Person {firstName = "name_7", lastName = "lastname_7", formerLastNames = [], age = 0, parents = [p6], idNumber = Just (Passport (1111, 111111))}

p8 = Person {firstName = "name_8", lastName = "lastname_8", formerLastNames = [], age = 0, parents = [p6], idNumber = Just (Passport (1111, 111111))}

p9 = Person {firstName = "name_9", lastName = "lastname_9", formerLastNames = [], age = 0, parents = [p4, p7], idNumber = Just (Passport (1111, 111111))}

p10 = Person {firstName = "name_10", lastName = "lastname_10", formerLastNames = [], age = 0, parents = [p5, p8], idNumber = Just (Passport (1111, 111111))}

p11 = Person {firstName = "name_11", lastName = "lastname_11", formerLastNames = [], age = 0, parents = [p5, p8], idNumber = Just (Passport (1111, 111111))}

p12 = Person {firstName = "name_12", lastName = "lastname_12", formerLastNames = [], age = 0, parents = [p18, p19], idNumber = Just (Passport (1111, 111111))}

p13 = Person {firstName = "name_13", lastName = "lastname_13", formerLastNames = [], age = 0, parents = [p11, p12], idNumber = Just (Passport (1111, 111111))}

p14 = Person {firstName = "name_14", lastName = "lastname_14", formerLastNames = [], age = 0, parents = [p20], idNumber = Just (Passport (1111, 111111))}

p15 = Person {firstName = "name_15", lastName = "lastname_15", formerLastNames = [], age = 0, parents = [p9, p14], idNumber = Just (Passport (1111, 111111))}

p16 = Person {firstName = "name_16", lastName = "lastname_16", formerLastNames = [], age = 0, parents = [p9, p14], idNumber = Just (Passport (1111, 111111))}

p17 = Person {firstName = "name_17", lastName = "lastname_17", formerLastNames = [], age = 0, parents = [p9, p14], idNumber = Just (Passport (1111, 111111))}

p18 = Person {firstName = "name_18", lastName = "lastname_18", formerLastNames = [], age = 102, parents = [], idNumber = Just (Passport (1111, 111111))}

p19 = Person {firstName = "name_19", lastName = "lastname_19", formerLastNames = [], age = 103, parents = [], idNumber = Just (Passport (1111, 111111))}

p20 = Person {firstName = "name_20", lastName = "lastname_20", formerLastNames = [], age = 104, parents = [p21], idNumber = Just (Passport (1111, 111111))}

p21 = Person {firstName = "name_21", lastName = "lastname_21", formerLastNames = [], age = 115, parents = [], idNumber = Just (Passport (1111, 111111))}

-- ┌────────────────────────────────────────────────────┐
-- │                                                    │
-- │  p21    p1──┬──p2         p6                       │
-- │   |         │              │                       │
-- │   |         │              │                       │
-- │   ▼         ▼              ▼                       │
-- │   p20    p3,p4,p5──────┬─p8,p7        p18──┬─p19   │
-- │    │        │          │     │             │       │
-- │    │        │          │     │             │       │
-- │    │        └─┬────────┼─────┘             │       │
-- │    │          │        │                   │       │
-- │    ▼          ▼        ▼                   ▼       │
-- │   p14───┬────p9     p10,p11──────┬────────p12      │
-- │         │                        │                 │
-- │         ▼                        ▼                 │
-- │    p15,p16,p17                  p13                │
-- │                                                    │
-- └────────────────────────────────────────────────────┘
greatestAncestorTests = [greatestAncestor p1 == p1, greatestAncestor p2 == p2, greatestAncestor p3 == p2, greatestAncestor p3 == p2, greatestAncestor p4 == p2, greatestAncestor p5 == p2, greatestAncestor p6 == p6, greatestAncestor p7 == p6, greatestAncestor p8 == p6, greatestAncestor p9 == p6, greatestAncestor p10 == p6, greatestAncestor p11 == p6, greatestAncestor p12 == p19, greatestAncestor p13 == p6, greatestAncestor p14 == p21, greatestAncestor p15 == p21, greatestAncestor p16 == p21, greatestAncestor p17 == p21, greatestAncestor p18 == p18, greatestAncestor p19 == p19, greatestAncestor p20 == p21, greatestAncestor p21 == p21]

ancestorsTests = [ancestors 1 p15 == Set.fromList [p9, p14], ancestors 2 p15 == Set.fromList [p4, p7, p20], ancestors 3 p15 == Set.fromList [p1, p2, p6, p21], ancestors 4 p15 == Set.empty, ancestors 1 p13 == Set.fromList [p11, p12], ancestors 2 p13 == Set.fromList [p5, p8, p18, p19]]

st = Set.fromList [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20, p21]

descendantsTests = [dt1, dt2, dt3, dt4, dt5, dt6]

dt1 = descendants p1 st == Node p1 [Leaf p3, Node p4 [Node p9 [Leaf p15, Leaf p16, Leaf p17]], Node p5 [Leaf p10, Node p11 [Leaf p13]]]

dt2 = descendants p2 st == Node p2 [Leaf p3, Node p4 [Node p9 [Leaf p15, Leaf p16, Leaf p17]], Node p5 [Leaf p10, Node p11 [Leaf p13]]]

dt3 = descendants p3 st == Leaf p3

dt4 = descendants p4 st == Node p4 [Node p9 [Leaf p15, Leaf p16, Leaf p17]]

dt5 = descendants p6 st == Node p6 [Node p7 [Node p9 [Leaf p15, Leaf p16, Leaf p17]], Node p8 [Leaf p10, Node p11 [Leaf p13]]]

dt6 = descendants p21 st == Node p21 [Node p20 [Node p14 [Leaf p15, Leaf p16, Leaf p17]]]

--RES OF TESTS--
res = and (greatestAncestorTests ++ ancestorsTests ++ descendantsTests)

--RES OF TESTS--

-- TEST SECTION --
