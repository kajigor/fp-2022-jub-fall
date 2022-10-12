module Test.Person where

import qualified Data.Set as Set
import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person

person1 :: Person
person1 =
  Person { firstName = "Gosha"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 29
         , idNumber = Just $ BirthCertificate (1, "aa", 567890)
         , parents = (Nothing, Nothing) }

person2 :: Person
person2 =
  Person { firstName = "Maria"
         , lastName = "Vernova"
         , formerLastNames = []
         , age = 30
         , idNumber = Just $ BirthCertificate (1, "ab", 399393)
         , parents = (Nothing, Nothing) }

person12child :: Person
person12child =
  Person { firstName = "Misha"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 0
         , idNumber = Just $ BirthCertificate (0, "aa", 000000)
         , parents = (Just $ person1, Just $ person2) }

person21child :: Person
person21child =
  Person { firstName = "Katya"
         , lastName = "Vernova"
         , formerLastNames = []
         , age = 0
         , idNumber = Just $ BirthCertificate (0, "aa", 000000)
         , parents = (Just $ person2, Just $ person1) }

personGranny :: Person
personGranny =
  Person { firstName = "Liza"
         , lastName = "Nekovich"
         , formerLastNames = []
         , age = 68
         , idNumber = Just $ BirthCertificate (2, "ss", 768493)
         , parents = (Nothing, Nothing) }

person3 :: Person
person3 =
  Person { firstName = "Yulia"
         , lastName = "Nekovich"
         , formerLastNames = []
         , age = 42
         , idNumber = Just $ BirthCertificate (9, "kf", 289389)
         , parents = (Just $ personGranny, Nothing) }

person4 :: Person
person4 =
  Person { firstName = "Yuriy"
         , lastName = "Belchenko"
         , formerLastNames = []
         , age = 41
         , idNumber = Just $ BirthCertificate (7, "dh", 238495)
         , parents = (Nothing, Nothing) }

person_3child :: Person
person_3child =
  Person { firstName = "Sasha"
         , lastName = "Nekovich"
         , formerLastNames = []
         , age = 0
         , idNumber = Just $ BirthCertificate (0, "aa", 000000)
         , parents = (Nothing, Just $ person3) }

person4_child :: Person
person4_child =
  Person { firstName = "Lilia"
         , lastName = "Belchenko"
         , formerLastNames = []
         , age = 0
         , idNumber = Just $ BirthCertificate (0, "aa", 000000)
         , parents = (Just $ person4, Nothing) }

person4__3child :: Person
person4__3child =
  Person { firstName = "Eugene"
         , lastName = "Belchenko"
         , formerLastNames = []
         , age = 0
         , idNumber = Just $ BirthCertificate (0, "aa", 000000)
         , parents = (Just $ person4_child, Just $ person_3child) }

allPeople :: Set.Set Person
allPeople = Set.fromList [person1, person2, person3, person4, person12child, person21child, person4_child, person_3child, person4__3child, personGranny]

unit_createChild = do
  createChild (Just $ person1, Just $ person2) "Misha" @?= person12child
  createChild (Just $ person2, Just $ person1) "Katya" @?= person21child
  createChild (Nothing, Just $ person3) "Sasha" @?= person_3child
  createChild (Just $ person4, Nothing) "Lilia" @?= person4_child
  createChild (Just $ person4_child, Just $ person_3child) "Eugene" @?= person4__3child

unit_greatestAncestor = do
    greatestAncestor person4__3child @?= personGranny
    greatestAncestor person4_child @?= person4
    greatestAncestor person_3child @?= personGranny
    greatestAncestor person12child @?= person2

unit_ancestors = do
    ancestors 1 person12child @?= Set.fromList [person1, person2]
    ancestors 2 person4__3child @?= Set.fromList [person3, person4]
    ancestors 3 person4__3child @?= Set.fromList [personGranny]

unit_descendants = do
    descendants person12child (allPeople) @?= Tree person12child Set.empty
    descendants person1 (allPeople) @?= Tree person1 (Set.fromList [(Tree person12child Set.empty), (Tree person21child Set.empty)])
    descendants personGranny (allPeople) @?= Tree personGranny (Set.fromList [Tree person3 (Set.fromList [Tree person_3child (Set.fromList [Tree person4__3child Set.empty])])])
    descendants person4 (allPeople) @?= Tree person4 (Set.fromList [Tree person4_child (Set.fromList [Tree person4__3child Set.empty])])