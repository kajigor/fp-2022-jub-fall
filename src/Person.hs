module Person where

import qualified Data.Set as Set

data Tree a = Node a [Tree a] deriving (Show, Eq, Ord)

data Document = Passport Int Int | BirthId String Int deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: (Maybe Person, Maybe Person) -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: String -> String -> (Maybe Person, Maybe Person) -> Person
createChild name surname = Person name surname [] 0 Nothing

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor = fst . greatestAncestorWithLevel where
  greatestAncestorWithLevel :: Person -> (Person, Int)
  greatestAncestorWithLevel person = case parents person of
    (Just par1, Just par2) ->
      let (a, aLevel) = greatestAncestorWithLevel par1 in
        let (b, bLevel) = greatestAncestorWithLevel par2 in
          if aLevel > bLevel || (aLevel == bLevel && age a > age(b))
            then (a, aLevel + 1)
            else (b, bLevel + 1)
    (Just par1, _) ->
      let (a, aLevel) = greatestAncestorWithLevel par1 in
        (a, aLevel + 1)
    (_, Just par2) ->
      let (a, aLevel) = greatestAncestorWithLevel par2 in
        (a, aLevel + 1)
    (_, _) ->
      (person, 0)


-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors d person = ancestorsMb d (Just person) where
  ancestorsMb :: Int -> Maybe Person -> Set.Set Person
  ancestorsMb dMb personMb = case (dMb, personMb) of
    (_, Nothing) -> Set.fromList []
    (0, Just p) -> Set.fromList [p]
    (dd, Just p) -> Set.union (ancestorsMb (dd - 1) (fst (parents p))) (ancestorsMb (dd - 1) (snd (parents p)))

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Set.Set Person -> Person -> Tree Person
descendants people person =
  let children = filter (\p -> fst (parents p) == Just person || snd (parents p) == Just person) (Set.toList people) in
    Node person (map (descendants people) children)