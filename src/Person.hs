module Person where


import Data.Function
import Data.Foldable
import qualified Data.Set as Set

data Tree a = Node a (Set.Set (Tree a))
              deriving(Show, Eq, Ord)

data Document = Passport (Int, Int) | BirthCertificate (String, Int)
              deriving(Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: (Set.Set Person)       -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: Set.Set Person -> String -> String -> Maybe Document -> Person
createChild par firstName lastName idNumber = 
  Person { 
    firstName = firstName, 
    lastName = lastName, 
    formerLastNames = [],
    age = 0,
    idNumber = idNumber, 
    parents = par
    }

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = (fst . helper) person
 where
   helper person | parents person == Set.empty = (person, 0) -- Если нет родителей, то самый старый потомок - он сам
   helper person = 
    let parentsCurrent = Set.map helper $ parents person in -- Считаем старшего потомка от каждого родителя
      let (person, lvl) = maximumBy (compare `on` (\(p, l) -> (l, age p))) parentsCurrent in-- Выбираем из них старшего и далёкого
        (person, lvl + 1)


-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors 0 person = Set.singleton person -- Предок человека уровня 0 - он сам
ancestors lvl person | lvl > 0 = Set.unions (Set.map (ancestors (lvl - 1)) (parents person))
ancestors _ _ = Set.empty -- Если уровень меньше 0

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
descendants person humans = do
  let children = Set.filter (\x -> Set.member person (parents x)) humans -- Берём детей данного человека
  let trees = Set.map (\x -> (descendants x humans)) children -- Строим по каждому дерево
  Node person trees