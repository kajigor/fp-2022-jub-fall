module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set

person1 :: Person
person1 =
  Person { firstName = "Pat"
         , lastName = "Guzman"
         , formerLastNames = []
         , age = 60
         , idNumber = Just(PassportNo (1234, 567890))
         , parents = [] }

person2 :: Person
person2 =
  Person { firstName = "Otis"
         , lastName = "Arnold"
         , formerLastNames = []
         , age = 61
         , idNumber = Just(PassportNo (2345, 678901))
         , parents = [] }

person3 :: Person
person3 =
  Person { firstName = "Rachelle"
         , lastName = "Watts"
         , formerLastNames = []
         , age = 62
         , idNumber = Just(PassportNo (3456, 789012))
         , parents = [] }


person4 :: Person
person4 =
  Person { firstName = "Diego"
         , lastName = "Long"
         , formerLastNames = []
         , age = 63
         , idNumber = Just(PassportNo (4567, 890123))
         , parents = [] }

person5 :: Person
person5 =
  Person { firstName = "Efrain"
         , lastName = "Castaneda"
         , formerLastNames = []
         , age = 30
         , idNumber = Just(PassportNo (5678, 901234))
         , parents = [person1, person2] }

person6 :: Person
person6 =
  Person { firstName = "Heath"
         , lastName = "Mendoza"
         , formerLastNames = []
         , age = 31
         , idNumber = Just(PassportNo (4567, 890123))
         , parents = [person3, person4] }

person7 :: Person
person7 =
  Person { firstName = "Ella"
         , lastName = "Cotton"
         , formerLastNames = []
         , age = 0
         , idNumber = Just(BirthCertificateNo ("KS", 55801))
         , parents = [person5, person6] }

person8 :: Person
person8 =
  Person { firstName = "Gail"
         , lastName = "Mitchell"
         , formerLastNames = []
         , age = 1
         , idNumber = Just(BirthCertificateNo ("NJ", 07051))
         , parents = [person5, person6] }


unit_create_child = do
  let child1 = createChild "Ivan" "Ivanov" 1 "III-AH" 222222 [person1, person2]
  let child2 = createChild "A" "AA" 0 "wdfry" 123456 [person1]
  let child3 = createChild "B" "BB" 2 "iowu" 234567 [person2]
  let child4 = createChild "C" "CC" 4 "hjgvb" 345678 []

  assertBool "valid" (parents child1 == [person1, person2])
  assertBool "valid" (parents child2 == [person1])
  assertBool "valid" (parents child3 == [person2])

unit_greatest_ancestor = do
  greatestAncestor person5 @?= person2
  greatestAncestor person6 @?= person4
  greatestAncestor person7 @?= person4
  greatestAncestor person8 @?= person4

unit_ancestors = do
  ancestors 0 person1 @?= Set.fromList [person1]
  ancestors 0 person5 @?= Set.fromList [person5]
  ancestors 0 person7 @?= Set.fromList [person7]

  ancestors 1 person5 @?= Set.fromList [person1, person2]
  ancestors 1 person6 @?= Set.fromList [person3, person4]
  ancestors 1 person7 @?= Set.fromList [person5, person6]
  ancestors 1 person8 @?= Set.fromList [person5, person6]

  ancestors 2 person7 @?= Set.fromList [person1, person2, person3, person4]
  ancestors 2 person8 @?= Set.fromList [person1, person2, person3, person4]
