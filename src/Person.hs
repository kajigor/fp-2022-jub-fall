module Person where

import Control.Monad
import qualified Data.Set as Set

data Passport = Passport {passNum :: (Int, Int)} deriving (Show, Eq)

data BirthCert = BirthCert {bcNum :: (String, Int)} deriving (Show, Eq)

data Document = Passport_ Passport | BirthCert_ BirthCert deriving (Show, Eq)

-- Тип данных для человека
data Person = Person
  { firstName :: String, -- Имя, должно быть непустым
    lastName :: String, -- Фамилия, должна быть непустой
    formerLastNames :: [String], -- Предыдущие фамилии, если фамилия менялась
    age :: Int, -- Возраст, должен быть неотрицательным
    idDoc :: Maybe Document, -- Какое-то удостоверение личности
    parents :: Set.Set Person -- Родители данного человека. Set так как все различны, доступ к каждому за одинаковое время (логарифм). И так как бывают семьи в которых <2 или >2 (мать+отец+отчем)
  }
  deriving (Show)

instance Eq Person where
  (==) p1 p2 = (idDoc p1) == (idDoc p2)

instance Ord Person where
  (<=) p1 p2 = (age p1) <= (age p2)

validateDoc :: Maybe Document -> Int -> Bool
validateDoc (Just (Passport_ p)) age =
  age >= 14
    && fst (passNum p) >= 0
    && fst (passNum p) <= 9999
    && snd (passNum p) >= 0
    && snd (passNum p) <= 999999
validateDoc (Just (BirthCert_ bc)) age =
  age < 14
    && length (fst (bcNum bc)) == 3
    && snd (bcNum bc) >= 0
    && snd (bcNum bc) <= 999999
validateDoc Nothing _ = True

-- Проверяем, что все родители старше ребенка
checkParents :: Person -> Bool
checkParents person = Set.null (Set.filter (person <=) (parents person))

-- Проверяем, что имя и фамилия не пустые, возраст >=0, документ если есть, то валидный, ребенок не старше родителей.
validatePerson :: Person -> Bool
validatePerson person
  | firstName person /= ""
      && lastName person /= ""
      && age person >= 0
      && validateDoc (idDoc person) (age person)
      && checkParents person =
    True
  | otherwise = False

-- Создание ребенка данных родителей. Возвращает Nothing если пытаемся создать невалидного ребенка, иначе Just.
createChild :: String -> String -> Int -> Maybe Document -> Set.Set Person -> Maybe Person
createChild newFirstName newLastName newAge newId newParents =
  if validatePerson newPerson
    then Just newPerson
    else Nothing
  where
    newPerson = Person newFirstName newLastName [] newAge newId newParents

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Maybe Person
greatestAncestor person | Set.null (parents person) = Nothing
greatestAncestor person = join $ Set.lookupMax (Set.map greatestAncestor (parents person))

unionSets :: Ord a => Set.Set a -> [Set.Set a] -> Set.Set a
unionSets acc [] = acc
unionSets set (s : xs) | Set.null set = unionSets s xs
unionSets acc (s : xs) = unionSets (Set.union acc s) xs

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors x _ | x <= 0 = Set.empty
ancestors 1 person = parents person
ancestors level person = unionSets Set.empty (map (ancestors (level -1)) (Set.toList (parents person)))

data Tree v = Empty | Node v [Tree v] deriving (Show, Eq, Ord)

isChild :: Person -> Person -> Bool
isChild parent person = Set.lookupGE parent (parents person) == Just parent

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Set.Set Person -> Person -> Tree Person
descendants allPeople person = Node person (Set.toList (Set.map (descendants allPeople) (Set.filter (isChild person) allPeople)))