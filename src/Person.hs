module Person where

import qualified Data.Set as Set

data Tree a = Tree
  { element :: a
  , children :: Set.Set (Tree a)
  }
  deriving (Show, Eq, Ord)

data Document = BirthCertificate (Int, String, Int) deriving (Show, Eq, Ord)

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
createChild :: (Maybe Person, Maybe Person) -> String -> Person
createChild parents name =
  case (fst parents) of
      Nothing ->
        case (snd parents) of
          Nothing -> 
            Person { firstName = name
                   , lastName = "undefined"
                   , formerLastNames = []
                   , age = 0
                   , idNumber = Just $ BirthCertificate (0, "aa", 000000)
                   , parents = parents}
          Just parent2 ->
            Person { firstName = name
                   , lastName = (lastName parent2)
                   , formerLastNames = []
                   , age = 0
                   , idNumber = Just $ BirthCertificate (0, "aa", 000000)
                   , parents = parents}
      Just parent1 ->
        Person { firstName = name
                , lastName = (lastName parent1)
                , formerLastNames = []
                , age = 0
                , idNumber = Just $ BirthCertificate (0, "aa", 000000)
                , parents = parents} -- always take the first parent lastname

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них

deeperAndOlder :: (Person, Int) -> (Person, Int) -> (Person, Int)
deeperAndOlder pair1 pair2 = 
  if ((snd pair1) < (snd pair2) || ((snd pair1) == (snd pair2) && (age $ fst $ pair1) > (age $ fst $ pair2)))
    then pair1
    else pair2

greatestAncestor :: Person -> Person
greatestAncestor person = fst $ greatestAncestorWithHeight person 0
  where 
    greatestAncestorWithHeight :: Person -> Int -> (Person, Int)
    greatestAncestorWithHeight person height =
      case (fst $ parents $ person) of
        Nothing ->
          case (snd $ parents $ person) of
            Nothing -> (person, 0)
            Just parent2 -> greatestAncestorWithHeight parent2 (height + 1)
        Just parent1 ->
          case (snd $ parents $ person) of
            Nothing -> greatestAncestorWithHeight parent1 (height + 1)
            Just parent2 -> deeperAndOlder (greatestAncestorWithHeight parent1 (height + 1)) (greatestAncestorWithHeight parent2 (height + 1))

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors level person = ancestorsWithHeight person 0 level
  where 
    ancestorsWithHeight :: Person -> Int -> Int -> Set.Set Person
    ancestorsWithHeight person height level =
      if height == level 
        then Set.singleton person
      else 
        case (fst $ parents $ person) of
        Nothing ->
          case (snd $ parents $ person) of
            Nothing -> Set.empty
            Just parent2 -> ancestorsWithHeight parent2 (height + 1) level
        Just parent1 ->
          case (snd $ parents $ person) of
            Nothing -> ancestorsWithHeight parent1 (height + 1) level
            Just parent2 -> Set.union (ancestorsWithHeight parent1 (height + 1) level) (ancestorsWithHeight parent2 (height + 1) level)


-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants person peopleSet = Tree person (Set.map descendantsWithoutSet (Set.filter isParent peopleSet))
  where
    isParent :: Person -> Bool
    isParent probablyChild = ((fst $ parents $ probablyChild) == (Just $ person)) || ((snd $ parents $ probablyChild) == (Just $ person))
    descendantsWithoutSet :: Person -> Tree Person 
    descendantsWithoutSet person1 = descendants person1 peopleSet 