{-# LANGUAGE RecordWildCards #-}
module Person where

import qualified Data.Set as Set

data Tree a = Leaf {val :: a} | LeftNode {left :: Tree a, val :: a} | RightNode {val :: a, right :: Tree a}
              | Node {left :: Tree a, val :: a, right :: Tree a} deriving (Show, Eq)

data Document = BirthCert {id :: Int} | Passport {ser :: Int, num :: Int} deriving (Show, Eq)

data Parents = Parents {father :: Maybe Person, mother :: Maybe Person} deriving (Show, Eq)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , document :: Document        -- Какое-то удостоверение личности
  , parents :: Parents       -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Eq)
instance Show Person where
  show Person{firstName = name} = name

instance Ord Person where
  (<=) per1 per2 = age per1 <= age per2


-- Создание ребенка данных родителей
createChild :: Person -> Person -> String -> Document -> Person
createChild father mother name doc@(BirthCert _) = Person {firstName = name, lastName = lastName father,
                                                                formerLastNames = [], age = 0, document = doc,
                                                                parents = Parents (Just father) (Just mother)}
createChild _ _ _ _ = error "You should provide Birth Certificate for your child"

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = snd (go (0,person)) where
  go :: (Int, Person) -> (Int, Person)
  go (n, Person {parents=(Parents (Just father) (Just mother))}) = max (go (n+1, father)) $ go (n+1, mother)
  go (n, Person {parents=(Parents Nothing (Just mother))}) = go (n+1, mother)
  go (n, Person {parents=(Parents (Just father) Nothing)}) = go (n+1, father)
  go (n, person) = (n, person)

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors level per = Set.fromList $ go [] level (Just per) where
  go :: [Person] -> Int -> Maybe Person -> [Person]
  go acc 0 (Just per) = per : acc
  go acc _ Nothing = acc
  go acc n (Just per) = go acc (n-1) (father $ parents per) ++ go acc (n-1) (mother $ parents per)

-- Возвращает семейное древо данного человека, описывающее его потомков.
-- Me
-- ├─ Father
-- │  ├─ Grandpa
-- │  └─ Granny
-- └─ Mother
descendants :: Person -> Tree Person
descendants person@Person {parents=(Parents (Just father) (Just mother))} = Node (descendants father) person (descendants mother)
descendants person@Person {parents=(Parents (Just father) Nothing)} = LeftNode (descendants father) person
descendants person@Person {parents=(Parents Nothing (Just mother))} = RightNode person (descendants mother)
descendants person = Leaf person
