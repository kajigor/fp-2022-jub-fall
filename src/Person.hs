{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Person where

import qualified Data.Set as Set
import qualified Data.Tree as Tree
import Data.Maybe (fromMaybe)

data Document =
    Passport { id :: (Int, Int) }
  | BirthCert { id :: (Int, Int) }
  deriving (Eq, Ord)

pattern DefaultDocument :: Document
pattern DefaultDocument = BirthCert (0, 0)

-- Тип данных для родителей. Если какой-то родитель неизвестен, то ставится Nothing
-- Красиво, функционально, для удобного обращения есть функции fatherOf, safeFatherOf и т. д.
-- Для удобного pattern matching'а есть паттерны
data Parents = Parents
  { mother :: Maybe Person
  , father :: Maybe Person
  }
  deriving (Eq)

pattern NoParents :: Parents
pattern NoParents = Parents { mother = Nothing, father = Nothing }

pattern ParentlessPerson :: Person
pattern ParentlessPerson <- Person { parents = NoParents }

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Document        -- Какое-то удостоверение личности
  , parents :: Parents          -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Eq)


pattern DefaultPerson :: Person
pattern DefaultPerson = Person 
  { firstName = ""
  , lastName = ""
  , formerLastNames = []
  , age = -1
  , idNumber = DefaultDocument
  , parents = NoParents
  }

instance Ord Person where
  compare :: Person -> Person -> Ordering
  compare a b = compare (idNumber a) (idNumber b)

fatherOf :: Person -> Maybe Person
fatherOf = father . parents

safeFatherOf :: Person -> Person
safeFatherOf = fromMaybe DefaultPerson . fatherOf

motherOf :: Person -> Maybe Person
motherOf = mother . parents

safeMotherOf :: Person -> Person
safeMotherOf = fromMaybe DefaultPerson . motherOf

-- Создание ребенка данных родителей
createChild :: String -> String -> Document -> Parents -> Person
createChild fname lname birthCert parents = Person
  { firstName = fname
  , lastName = lname
  , formerLastNames = []
  , age = 0
  , idNumber = birthCert
  , parents = parents
  }

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = snd $ greatestAncestorInternal (0, person)
  where
    greatestAncestorInternal :: (Int, Person) -> (Int, Person)
    greatestAncestorInternal (n, p@ParentlessPerson) = (n, p)
    greatestAncestorInternal (n, p) =
      max (greatestAncestorInternal (n+1, safeMotherOf p)) (greatestAncestorInternal (n+1, safeFatherOf p))
      
    max :: (Int, Person) -> (Int, Person) -> (Int, Person)
    max a@(n1, p1) b@(n2, p2) = if n1 > n2 || age p1 > age p2 then a else b


-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors _ DefaultPerson = Set.empty
ancestors 0 p = Set.singleton p
ancestors n p = Set.union (ancestors (n-1) (safeMotherOf p)) (ancestors (n-1) (safeFatherOf p))


-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree.Tree Person
descendants person set = Tree.unfoldTree buildNode (person, set) 
  where
    buildNode :: (Person, Set.Set Person) -> (Person, [(Person, Set.Set Person)])
    buildNode (p, set) = (p, transformSet p set)

    transformSet :: Person -> Set.Set Person -> [(Person, Set.Set Person)]
    transformSet p set = map (mapWithSet set) . Set.toList . Set.filter (isChild p) $ set

    isChild :: Person -> Person -> Bool
    isChild parent person = safeMotherOf person == parent || safeFatherOf person == parent

    mapWithSet :: Set.Set Person -> Person -> (Person, Set.Set Person)
    mapWithSet set person = (person, set)
