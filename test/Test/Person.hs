module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set

parent1 :: Person
parent1 =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = PassportNo (1234, 567890)
         , parents = [] }

parent2 :: Person
parent2 =
  Person { firstName = "Aboba"
         , lastName = "Abober"
         , formerLastNames = []
         , age = 30
         , idNumber = Just (PassportNo (2345, 678900))
         , parents = [] }

parent12 :: Person
parent12 =
  Person { firstName = "Persona"
         , lastName = "Important"
         , formerLastNames = []
         , age = 9
         , idNumber = Just (BirthCertificateNo ("ABCD", 789000))
         , parents = [Just parent1, Just parent2] }


parent21 :: Person
parent21 =
  Person { firstName = "Kirill"
         , lastName = "Voronzov"
         , formerLastNames = []
         , age = 10
         , idNumber = Just (BirthCertificateNo ("QWER", 890000))
         , parents = [Just parent1, Just parent2] }

unit_create_child = do
  let child1 = createChild "Ivan" "Ivanov" 1 "III-AH" 222222 [Just parent1, Just parent2]
  let child2 = createChild "A" "AA" 0 "wdfry" 123456 [Just parent1]
  let child3 = createChild "B" "BB" 2 "iowu" 234567 [Just parent2]
  let child4 = createChild "C" "CC" 4 "hjgvb" 345678 []

  assertBool "valid" (parents child1 == [parent1, parent2])

unit_greatest_ancestor = do
  greatestAncestor parent12 @?= person2
  greatestAncestor parent1 @?= person1
  greatestAncestor parent2 @?= person2

unit_ancestors = do
  ancestors 1 person12 @?= Set.fromList [person1, person2]
  ancestors 1 person21 @?= Set.fromList [person1, person2]
  ancestors 0 person1 @?= Set.fromList [person1]
  ancestors 0 person2 @?= Set.fromList [person2]
