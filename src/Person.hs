module Person where

import qualified Data.Set as Set

data Tree = Tree
  { person :: Person
  , children :: [Tree]
  }
  deriving (Show, Eq)

data Document = PassportNo (Int, Int) | BirthCertificateNo (String, Int) deriving (Show, Eq)

-- Тип данных для человека
data Person = Person
  { firstName :: String            -- Имя, должно быть непустым
  , lastName :: String             -- Фамилия, должна быть непустой
  , formerLastNames :: [String]    -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                     -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document     -- Какое-то удостоверение личности
  , parents :: [Person]            -- Родители данного человека
  }
  deriving (Show, Eq)

instance Ord Person where
--  compare :: Person -> Person -> Ordering
  compare a b =  compare (age a, firstName a, lastName a) (age b, firstName b, lastName b)

-- Создание ребенка данных родителей
createChild :: String -> String -> Int -> String -> Int -> [Person] -> Person
createChild firstName lastName age idNumberSeries idNumberNo parents =
  Person firstName lastName [] age (Just (BirthCertificateNo (idNumberSeries, idNumberNo))) parents

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = snd $ greatestAncestor' (0, person) where
    greatestAncestor' (n, p) = case parents p of
      [] -> (n, p)
      ps -> foldl1 max $ map (\x -> greatestAncestor' (n + 1, x)) ps

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors k person = ancestors' k $ Set.singleton person where
  ancestors' :: Int -> Set.Set Person -> Set.Set Person
  ancestors' 0 people = people
  ancestors' n people = ancestors' (n - 1) $ foldl (\set p -> Set.union set (Set.fromList $ parents p)) Set.empty people

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree
descendants pers people = head $ buildTree (map Set.toList $ generations [Set.singleton pers] people) [] where

  generations :: [Set.Set Person] -> Set.Set Person -> [Set.Set Person]
  generations (g:gs) others = if (null others)
    then (g:gs)
    else let (gNew, others') = Set.partition (\p -> any (\x -> Set.member x g) $ parents p) others
      in generations (gNew:g:gs) others'

  buildTree :: [[Person]] -> [Tree] -> [Tree]
  buildTree [] ts     = ts
  buildTree (g:gs) ts = let ts' = map (\p -> Tree p $ filter (\x -> (elem p) (parents $ person x)) ts) g
    in buildTree gs ts'