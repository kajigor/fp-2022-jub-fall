module Test.Person where

import Data.Set (Set)
import qualified Data.Set as Set

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person

-- =========================================================================
-- |     Семейной дерево Иосифа Сталина (на глубину один вверх и вниз)     |
-- =========================================================================
--
--                  grand1-----grand2     grand3
--                          |               |
--       parent3---------parent1---------parent2
--                  |               |
--            -------------         |
--            |           |         | 
--          child2      child3    child1
--



grand1 :: Person
grand1 = Person { firstName = "Besarion"
                 , lastName = "Jughashvili"
                 , formerLastNames = []
                 , age=172
                 , idNumber = Nothing
                 , parents = Set.empty }

grand2 :: Person
grand2 = Person { firstName = "Ekaterine"
                 , lastName = "Geladze"
                 , formerLastNames = []
                 , age=166
                 , idNumber = Just (Passport (1111, 111111))
                 , parents = Set.empty }

grand3 :: Person
grand3 = Person { firstName = "Semon"
                 , lastName = "Svanidze"
                 , formerLastNames = []
                 , age=177                 -- это направда скорее всего, но его возраста я не нашел
                 , idNumber = Nothing
                 , parents = Set.empty }

parent1 :: Person
parent1 = Person { firstName = "Joseph"
                  , lastName = "Stalin"
                  , formerLastNames = ["Jughashvili"]
                  , age=144
                  , idNumber = Just (Passport (1234, 123456))
                  , parents = Set.fromList [grand1, grand2] }

parent2 :: Person
parent2 = Person { firstName = "Ekaterine"
                  , lastName = "Svanidze"
                  , formerLastNames = []
                  , age=137
                  , idNumber = Nothing
                  , parents = Set.singleton grand3 }

parent3 :: Person
parent3 = Person { firstName = "Nadezhda"
                  , lastName = "Alliluyeva"
                  , formerLastNames = []
                  , age=121
                  , idNumber = Just (Passport (2222, 222222))
                  , parents = Set.empty }

child1 :: Person
child1 = Person { firstName = "Yakov"
                  , lastName = "Jugashvili"
                  , formerLastNames = []
                  , age=115
                  , idNumber = Just (Passport (1337, 222111))
                  , parents = Set.fromList [parent1, parent2] }

child2 :: Person
child2 = Person { firstName = "Vasily"
                  , lastName = "Stalin"
                  , formerLastNames = []
                  , age=101
                  , idNumber = Just (Passport (1232, 842389))
                  , parents = Set.fromList [parent1, parent3] }

child3 :: Person
child3 = Person { firstName = "Nadezhda"
                  , lastName = "Alliluyeva"
                  , formerLastNames = []
                  , age=96
                  , idNumber = Just (Passport (2222, 222222))
                  , parents = Set.fromList [parent1, parent3] }

-- =========================================================================

testParent1 :: Person
testParent1 = Person { firstName = "Volodia"
                  , lastName = "Lenin"
                  , formerLastNames = ["Ulianov"]
                  , age=10
                  , idNumber = Nothing
                  , parents = Set.empty }

testParent2 :: Person
testParent2 = Person { firstName = "Alexander"
                  , lastName = "Kerensky"
                  , formerLastNames = []
                  , age=11
                  , idNumber = Nothing
                  , parents = Set.empty }


testChild1 :: Person
testChild1 = Person { firstName = "Yakov"
                  , lastName = "Jugashvili"
                  , formerLastNames = []
                  , age=0
                  , idNumber = Nothing
                  , parents = Set.fromList [parent1, parent2] }

testChild2 :: Person
testChild2 = Person { firstName = "Rayan"
                  , lastName = "Gosling"
                  , formerLastNames = []
                  , age=0
                  , idNumber = Nothing
                  , parents = Set.empty}

testChild3 :: Person
testChild3 = Person { firstName = "Jesus"
                  , lastName = "Christ"
                  , formerLastNames = []
                  , age=2022
                  , idNumber = Nothing
                  , parents = Set.fromList [testParent1, testParent2]}

testChild4 :: Person
testChild4 = Person { firstName = "Aristotle"
                  , lastName = "from Greece (hopefully)"
                  , formerLastNames = []
                  , age=0
                  , idNumber = Nothing
                  , parents = Set.singleton testParent1}

-- =========================================================================

wholeFamily = Set.fromList [grand1, grand2, grand3, parent1, parent2, child1, child2, child3]

stalinsDescendants = Tree parent1 (Set.fromList [Tree child1 Set.empty, Tree child2 Set.empty, Tree child3 Set.empty])
grand1Descendants = Tree grand1 (Set.singleton stalinsDescendants)
grand3Descendants = Tree grand3 (Set.singleton (Tree parent2 (Set.singleton (Tree child1 Set.empty))))


unit_general = do
  False @?= testChild1 == testChild2                                                 -- НЕ равны
  True @?= testChild1 <= testChild2 && testChild2 <= testChild1                      -- НО такое выполняется, потому что сравниваем по возрасту
                                                                                     -- (автору за это стыдно, но исправлять тоже не хочется)

unit_createChild = do
  testChild1 @?= createChild "Yakov" "Jugashvili" (Set.fromList [parent1, parent2])
  testChild2 @?= createChild "Rayan" "Gosling" Set.empty
  testChild4 @?= createChild "Aristotle" "from Greece (hopefully)" (Set.singleton testParent1) 


unit_greatestAncestor = do
  grand1 @?= greatestAncestor parent1
  parent3 @?= greatestAncestor parent3
  grand3 @?= greatestAncestor child1
  grand1 @?= greatestAncestor child2
  testParent2 @?= greatestAncestor testChild3


unit_ancestors = do
  Set.fromList [grand1, grand2] @?= ancestors 1 parent1
  Set.fromList [grand1, grand2, grand3] @?= ancestors 2 child1
  Set.empty @?= ancestors 3 child1
  Set.singleton child1 @?= ancestors 0 child1
  Set.empty @?= ancestors (-1) child1


unit_descendants = do
  Tree child1 Set.empty @?= descendants child1 wholeFamily
  stalinsDescendants @?= descendants parent1 wholeFamily
  grand1Descendants @?= descendants grand1 wholeFamily
  grand3Descendants @?= descendants grand3 wholeFamily


