module Person where

import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

data Tree a = Empty
            | Node a [Tree a]
            deriving (Show, Eq, Ord)

data Document = Passport (Int, Int)
              | BirthCert Int
              deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , document :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: (Maybe Person, Maybe Person)       -- Родители данного человека. Пара это просто и относительно удобно, хоть и привносит проблемы с порядком родителей.
  } -- We still need to somehow use persons without parents, i.e. in tests. Also we believe that there are only two parents for a child
  deriving (Show, Eq, Ord) -- На самом деле с каждой правкой этот код всё больше становится пороховой бочкой, что держится на честном слове

-- emptyDebugPerson :: Person
-- emptyDebugPerson = Person {firstName = "Bad", lastName = "Design", formerLastNames = [], age = 0, document = Nothing, parents = (Nothing, Nothing)}

-- Создание ребенка данных родителей
createChild :: Person -> Person -> String -> Person -- Parents and the name of the child
createChild parent1 parent2 name = Person { firstName = name, lastName = (lastName parent1) ++ ('-' : (lastName parent2)), formerLastNames = [], age = 0, document = Nothing, parents = ((Just parent1), (Just parent2))}

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = Maybe.fromMaybe undefined (snd (greatestAncestorWithLevel (1, Just person))) -- at least one person is present
  where
    greatestAncestorWithLevel :: (Int, Maybe Person) -> (Int, Maybe Person)
    greatestAncestorWithLevel (_, Nothing) = (0, Nothing)
    greatestAncestorWithLevel (n, Just person) = if (parents person) == (Nothing, Nothing) then (n, Just person) else max (greatestAncestorWithLevel (n + 1, fst (parents person))) (greatestAncestorWithLevel (n + 1, snd (parents person)))

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors level person = maybeAncestors level (Just person)

maybeAncestors :: Int -> Maybe Person -> Set.Set Person
maybeAncestors _ Nothing = Set.empty
maybeAncestors 0 (Just person) = Set.singleton person
maybeAncestors level (Just person) = Set.union (maybeAncestors (level - 1) (fst (parents person))) (maybeAncestors (level - 1) (snd (parents person)))

isChildOf :: Person -> Person -> Bool
isChildOf parent possibleChild = (fst (parents possibleChild)) == (Just parent) || (snd (parents possibleChild)) == (Just parent)

-- Возвращает семейное древо данного человека, описывающее его потомков.
-- Поменял порядок аргументов для использования descendants people в Set.map
descendants :: Set.Set Person -> Person -> Tree Person
descendants people person = Node person (Set.elems (Set.map (descendants people) (Set.filter (isChildOf person) people)))

