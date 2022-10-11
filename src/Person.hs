module Person (Tree(..), BirthCertificate(..), Passport(..), Document(..), Person(..), createChild, greatestAncestor, ancestors, descendants) where

import qualified Data.Set as Set

data Tree = Tree
  { thisPerson :: Person
  , allDescendants :: Set.Set Tree } -- Для каждого ребёнка будет своё дерево, что-то типа рекурсии,
                                       -- то есть количество деревьев в множестве - количество детей
  deriving (Show, Eq, Ord)

data BirthCertificate = BirthCertificate
  { birthCertSeries :: String
  , birthCertNumber :: Int }
  deriving (Show, Eq, Ord)

data Passport = Passport
  { passportSeries :: Int
  , passportNumber :: Int }
  deriving (Show, Eq, Ord)

data Document = BirthCertificate_ BirthCertificate | Passport_ Passport deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String                     -- Имя, должно быть непустым
  , lastName :: String                      -- Фамилия, должна быть непустой
  , formerLastNames :: [String]             -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                              -- Возраст, должен быть неотрицательным
  , idDocument :: Document                  -- Какое-то удостоверение личности
  , parents :: (Maybe Person, Maybe Person) -- Родители данного человека
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: Maybe Person -> Maybe Person -> String -> String -> Document -> Person -- Родитель 1, Родитель 2, Имя, Фамилия, Документ
createChild (Just parent1) (Just parent2) firstName_ lastName_ document =
  Person { firstName = firstName_
  , lastName = lastName_
  , formerLastNames = []
  , age = 0
  , idDocument = document
  , parents = (Just parent1, Just parent2) }
createChild (Just parent1) Nothing firstName_ lastName_ document =
  Person { firstName = firstName_
  , lastName = lastName_
  , formerLastNames = []
  , age = 0
  , idDocument = document
  , parents = (Just parent1, Nothing) }
createChild Nothing (Just parent2) firstName_ lastName_ document =
  Person { firstName = firstName_
  , lastName = lastName_
  , formerLastNames = []
  , age = 0
  , idDocument = document
  , parents = (Nothing, Just parent2) }
createChild Nothing Nothing _ _ _ = undefined

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person_ =
  go person_ (parents person_) where
    go person (Nothing, Nothing) = person
    go _ (Just parent1, Nothing) = go parent1 (parents parent1)
    go _ (Nothing, Just parent2) = go parent2 (parents parent2)
    go _ (Just parent1, Just parent2) | age ancestor1 > age ancestor2 = ancestor1
                              | otherwise = ancestor2 where
                                ancestor1 = go parent1 (parents parent1)
                                ancestor2 = go parent2 (parents parent2)

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors level person =
  go level (Just person) Set.empty where
    go 0 (Just person_) acc = Set.insert person_ acc
    go _ (Nothing) acc = acc
    go level_ (Just person_) acc = Set.union (go (level_ - 1) (fst (parents person_)) acc) (go (level_ - 1) (snd (parents person_)) acc)

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree
-- Выбираем детей данного человека из множества всех людей, для них выполняем рекурсивно эту же функцию (ищем потомков)
descendants person allPeople = Tree person (Set.map (`descendants` allPeople) (findChildren person allPeople)) where
  findChildren person_ allPeople_ = Set.filter (isChild person_) allPeople_ where
    isChild parent child =
      go parent (parents child) where
        go _ (Nothing, Nothing) = False
        go parent_ (Just parent1, Nothing) = parent_ == parent1
        go parent_ (Nothing, Just parent2) = parent_ == parent2
        go parent_ (Just parent1, Just parent2) = parent_ == parent1 || parent_ == parent2
