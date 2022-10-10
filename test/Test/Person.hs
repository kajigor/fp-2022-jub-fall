module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set

person1 :: Person
person1 =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 56
         , idNumber = Just (Passport (1234, 567890))
         , parents = Set.empty }

person2 :: Person
person2 =
  Person { firstName = "Yuri"
         , lastName = "Verbitskiy"
         , formerLastNames = []
         , age = 59
         , idNumber = Just (Passport (1234, 567820))
         , parents = Set.empty }

person3 :: Person
person3 =
  Person { firstName = "Alena"
         , lastName = "Arama"
         , formerLastNames = []
         , age = 84
         , idNumber = Just (Passport (1234, 186183))
         , parents = Set.empty }

person4 = (createChild (Set.fromList [person1, person2]) "Artem" "Verbistkiy") {age = 32}

person5 = (createChild (Set.fromList [person3]) "Elena" "Arama") {age = 60}

person6 = (createChild (Set.fromList [person5]) "Ekaterina" "Arama") {age = 28}

person7 = (createChild (Set.fromList [person4, person6]) "Anna" "Verbitskaia") {age = 28}

person8 = (createChild (Set.fromList [person3]) "Anna" "Trott") {age = 58}

setAll = Set.fromList [person1, person2, person3, person4, person5, person6, person7, person8]

unit_ancestors = do
  ancestors 1 person7 @?= Set.fromList [person4, person6]
  ancestors 2 person7 @?= Set.fromList [person1, person2, person5]
  ancestors 3 person7 @?= Set.fromList [person3]
  ancestors 1 person4 @?= Set.fromList [person1, person2]
  ancestors 2 person6 @?= Set.fromList [person3]

unit_greatestAncestor = do
  greatestAncestor person1 @?= person1
  greatestAncestor person7 @?= person3
  greatestAncestor person4 @?= person2
  greatestAncestor person8 @?= person3
  greatestAncestor person6 @?= person3

unit_descendants = do
  descendants person1 setAll @?= Tree person1 (Set.fromList[Tree person4 (Set.fromList [Tree person7 Set.empty])])
  descendants person2 setAll @?= Tree person2 (Set.fromList[Tree person4 (Set.fromList [Tree person7 Set.empty])])
  descendants person4 setAll @?= Tree person4 (Set.fromList [Tree person7 Set.empty])
  descendants person7 setAll @?= Tree person7 Set.empty
  descendants person3 setAll @?= Tree person3 (Set.fromList[(Tree person5 (Set.fromList [Tree person6 (Set.fromList[Tree person7 Set.empty])])), (Tree person8 Set.empty)])
  descendants person8 setAll @?= Tree person8 Set.empty