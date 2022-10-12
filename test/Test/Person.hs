module Test.Person where

import qualified Data.Set as Set
import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person

person0 :: Person
person0 =
  Person {firstName = "Oldest", lastName = "Unreal", formerLastNames = ["Real"], age = 6000000, document = Nothing, parents = (Nothing, Nothing)}

person1 :: Person
person1 = 
  Person {firstName = "Test", lastName = "Sample", formerLastNames = [], age = 1337, document = Just (Passport (1234, 567890)), parents = (Just person0, Nothing)}


person2 :: Person
person2 = 
  Person {firstName = "Check", lastName = "Text", formerLastNames = [], age = 800, document = Just (Passport (2233, 444000)), parents = (Nothing, Nothing)}

child1_2_1 :: Person
child1_2_1 =
  Person {firstName = "Lorem", lastName = "Sample-Text", formerLastNames = [], age = 0, document = Nothing, parents = (Just person1, Just person2)}

child1_2_2 :: Person
child1_2_2 =
  Person {firstName = "Ipsum", lastName = "Text-Sample", formerLastNames = [], age = 0, document = Nothing, parents = (Just person2, Just person1)}

leute :: Set.Set Person
leute = Set.fromList [person0, person1, person2, child1_2_1, child1_2_2]

unit_createChild = do
  createChild person1 person2 "Lorem" @?= child1_2_1
  createChild person2 person1 "Ipsum" @?= child1_2_2

unit_greatestAncestor = do
  greatestAncestor child1_2_1 @?= person0
  greatestAncestor (greatestAncestor child1_2_2) @?= person0

unit_ancestors = do
  ancestors 0 child1_2_1 @?= Set.fromList [child1_2_1]
  ancestors 1 child1_2_1 @?= Set.fromList [person1, person2]
  ancestors 2 child1_2_2 @?= Set.fromList [person0]


unit_descendants = do
  descendants leute child1_2_1 @?= Node child1_2_1 []
  descendants leute child1_2_2 @?= Node child1_2_2 []
  descendants leute person2 @?= Node person2 [(Node child1_2_2 []), (Node child1_2_1 [])]
