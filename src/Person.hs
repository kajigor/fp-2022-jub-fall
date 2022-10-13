module Person where

import qualified Data.Set as Set
import Data.Maybe

data Tree a = Node a (Set.Set (Tree a))
              deriving(Show, Eq, Ord)


data Document = Passport (Int, Int) | Bc (Int, String) deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: (Maybe Person, Maybe Person)       -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: (Maybe Person, Maybe Person) -> String -> Person
createChild (Just father, Just mother) name = Person name (lastName father) [] 0 Nothing (Just father, Just mother)
createChild (Just father, Nothing) name = Person name (lastName father) [] 0 Nothing (Just father, Nothing)
createChild (Nothing, Just mother) name = Person name (lastName mother) [] 0 Nothing (Nothing, Just mother)
createChild (Nothing, Nothing) name = Person name "Sirota" [] 0 Nothing (Nothing, Nothing)

-- -- Самый далекий предок данного человека.
-- -- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- -- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
-- greatestAncestor :: Person -> Person



-- -- Предки на одном уровне иерархии.
-- ancestors :: Int -> Person -> Set.Set Person


isChild :: Person -> Person -> Bool
isChild person child = fst (parents child) == Just person || snd (parents child) == Just person


-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Set.Set Person -> Person -> Tree Person
descendants people person =
    let children = Set.filter (isChild person) people in
        if (null children)
            then Node person Set.empty
        else Node person (Set.map (descendants people) children)
