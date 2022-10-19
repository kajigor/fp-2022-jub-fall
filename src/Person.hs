module Person (Person (..), Tree (..), Document (..), createChild, greatestAncestor, ancestors, descendants) where
import qualified Data.Set as Set
import Data.Foldable

data Tree a = Leaf a [Tree a]
  | Node a (Maybe (Tree a)) (Maybe (Tree a)) [Tree a]
  deriving (Show, Eq)

data Document = Passport (Int, Int) | BirthCertificate (String, Int)
  deriving (Show, Eq, Ord)



-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: (Maybe Person, Maybe Person)   -- Родители данного человека. Выбрать подходящий контейнер.
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: Maybe Person -> Maybe Person -> String -> String -> Maybe Document -> Person
createChild mother father firstName lastName idNumber = Person { firstName = firstName, lastName = lastName, formerLastNames = [], age = 0, idNumber = idNumber, parents = ( mother, father)}

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor p = snd (maxi $ list p) where
  list = getLeafs . getTree 
  maxi:: [(Int, Person)] -> (Int, Person)
  maxi [] = (-1, nilPerson)
  maxi (x:xs) = do
    let acc = (maxi xs)
    if fst x > fst acc || (fst x == fst acc && (age $ snd x) >= (age $ snd acc)) then
      x
    else acc

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors num p = Set.fromList $ map (\(i, x) -> x) $ filter (\(i, x) -> i == num)(getNodes . getTree $ p)

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Person -> Set.Set Person -> Tree Person
-- descendants = undefined
descendants p everyone = Leaf p (map (\x -> descendants x everyone) (children p everyone)) where
  children :: Person -> Set.Set Person -> [Person]
  children p everyone = toList $ Set.filter (\s -> Just p == fst (parents s) || Just p == snd (parents s)) everyone


-- utility --


nilPerson = Person {firstName = "Null", lastName = "Null", formerLastNames = [], age = -1,idNumber = Nothing, parents = (Nothing, Nothing) }

getTree:: Person -> Tree Person
getTree p = do
  case parents p of
    (Nothing, Nothing) -> Leaf p []
    (Just m, Nothing) -> Node p (Just $ getTree m) Nothing []
    (Nothing, Just f) -> Node p Nothing (Just $ getTree f) []
    (Just m, Just f) -> Node p (Just $ getTree m) (Just $ getTree f) []

getLeafs:: Tree Person -> [(Int, Person)]
getLeafs t = go [] (Just t) 0 where
  go:: [(Int, Person)] -> Maybe (Tree Person) -> Int -> [(Int, Person)]
  go acc Nothing _ = acc
  go acc (Just (Leaf p [])) num = (num, p):acc
  go acc (Just (Node k m f [])) num = (go acc m (num+1)) ++ (go acc f (num+1))

getNodes:: Tree Person -> [(Int, Person)]
getNodes t = go [] (Just t) 0 where
  go:: [(Int, Person)] -> Maybe (Tree Person) -> Int -> [(Int, Person)]
  go acc Nothing _ = acc
  go acc (Just (Leaf p [])) num = (num, p):acc
  go acc (Just (Node k m f [])) num = (num, k):(go acc m (num+1)) ++ (go acc f (num+1))

