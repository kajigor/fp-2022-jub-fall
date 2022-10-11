module Person where

import qualified Data.Set as Set

data Tree a = Node a (Set.Set (Tree a))
    deriving (Show, Eq, Ord)

data Document = Passport (Int, Int)   -- Номер паспорта: состоит из серии и номера.
    | BirthSertificate ([Char], Int)  -- Номер свидетельства о рождении: состоит из серии и номера.
    deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: Set.Set Person   -- Родители данного человека. Сет, так как порядок родителей нам не важен.
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: Set.Set Person -> String -> String -> Person
createChild p f l = Person {firstName = f, lastName = l, formerLastNames = [],
                                                          age = 0, idNumber = Nothing, parents = p}  -- При рождении у ребёнка ещё нет документа

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor p = getLast (greatestAncestorRecursion p) -- Воспользуемся вспомогательной функцией, чтобы рекурсивно найти самого далекого предка
          where getLast (_, _, c) = c

greatestAncestorRecursion :: Person -> (Int, Int, Person)
greatestAncestorRecursion p = if Set.null (parents p) then (0, -(age p), p)
          else decreaseFirst (Set.findMin (Set.map greatestAncestorRecursion (parents p)))
          where decreaseFirst (a, b, c) = (a - 1, b, c)

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors 0 p = Set.singleton p
ancestors h p = Set.unions (Set.map (ancestors (h - 1)) (parents p))

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants p s = Node p (Set.map (`descendants` s) (Set.filter (Set.member p . parents) s))