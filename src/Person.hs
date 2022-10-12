module Person where

import qualified Data.Set as Set

data Tree v = Leaf v | Node v (Set.Set (Tree v)) deriving (Show, Eq, Ord)

data Document = Passport(Int, Int) | BirthCertificateeId  (Int, String, Int) deriving (Show, Eq, Ord)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Maybe Document  -- Какое-то удостоверение личности
  , parents :: (Maybe Person,  Maybe Person) -- Родители данного человека, мама и папа(или их аналоги)
  }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild :: Maybe Person -> Maybe Person -> String -> String -> Person
createChild Nothing Nothing name lastName = Person {
  firstName = name,
  lastName = lastName,
  formerLastNames = [],
  age = 0,
  idNumber = Nothing,
  parents = (Nothing, Nothing)
}

createChild (Just parent1) Nothing name lastName = Person {
  firstName = name,
  lastName = lastName,
  formerLastNames = [],
  age = 0,
  idNumber = Nothing, 
  parents = (Just parent1, Nothing)
}

createChild Nothing (Just parent2) name lastName = createChild (Just parent2) Nothing name lastName

createChild (Just parent1) (Just parent2) name lastName = Person {
  firstName = name,
  lastName = lastName,
  formerLastNames = [],
  age = 0, 
  idNumber = Nothing,
  parents = (Just parent1, Just parent2)
}

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
greatestAncestor :: Person -> Person
greatestAncestor person = fst $ greatestAncestorWithDepth person
  where 
    greatestAncestorWithDepth :: Person -> (Person, Int)
    greatestAncestorWithDepth human = addDepth $ case parents human of 
      (Nothing, Nothing) -> (human, 0)
      (Nothing, Just b) -> greatestAncestorWithDepth b
      (Just a, Nothing) -> greatestAncestorWithDepth a
      (Just a, Just b) -> 
        let (aAncestor, aDepth) = greatestAncestorWithDepth a
          in let (bAncestor, bDepth) = greatestAncestorWithDepth b
            in if (aDepth, age aAncestor) < (bDepth, age bAncestor)
              then (bAncestor, bDepth)
              else (aAncestor, aDepth)
    addDepth (a, b) = (a, b + 1)

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors 0 person = Set.singleton person
ancestors lvl person | lvl < 0  = Set.empty
                     | otherwise = Set.union (getAnc(fst $ parents person)) (getAnc(snd $ parents person))
                     where 
                       getAnc anc = case anc of
                         Just anc -> ancestors (lvl - 1) anc
                         Nothing -> Set.empty

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Set.Set Person -> Person -> Tree Person
descendants allPersons person = case length children of
  0 -> Leaf person
  _ -> Node person (Set.fromList (map (descendants allPersons) children))
  where
    children = Set.toList (Set.filter isChild allPersons)
    isChild human = case parents human of 
      (Nothing, Nothing) -> False
      (Just a, Just b) -> a == person || b == person
      (Just a, Nothing) -> a == person
      (Nothing, Just b) -> b == person
    