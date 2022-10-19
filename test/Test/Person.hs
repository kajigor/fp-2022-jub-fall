module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set
import Data.Foldable

person1 :: Person
person1 =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = Just $ Passport (1234, 567890)
         , parents = (Nothing, Nothing) }

person2 :: Person
person2 =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42
         , idNumber = Just $ Passport (9876, 543210)
         , parents = (Nothing, Nothing) }

person3 :: Person
person3 =
  Person { firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21
         , idNumber = Just $ Passport (2121, 212121)
         , parents = ((Just person4), Nothing) }

person4 :: Person
person4 =
  Person { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 83
         , idNumber = Just $ Passport (1111, 111111)
         , parents = (Nothing, (Just person5)) }

person5 :: Person
person5 =
  Person { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 83
         , idNumber = Just $ Passport (1111, 111111)
         , parents = (Nothing, Nothing) }

child1 :: Person
child1 =
  Person { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 0
         , idNumber = Just $ BirthCertificate ("ab", 123456)
         , parents = ((Just person1), (Just person2)) }

child2 :: Person
child2 =
  Person { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 0
         , idNumber = Just $ BirthCertificate ("cd", 987654)
         , parents = (Nothing, Nothing) }
child3 :: Person
child3 =
  Person { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 0
         , idNumber = Just $ BirthCertificate ("ef", 121212)
         , parents = ((Just person1), (Just person2)) }


unit_createChild = do
  createChild (Just person1) (Just person2) "Ivan" "Ivanov" (Just $ BirthCertificate ("ab", 123456)) @?= child1
  createChild Nothing Nothing "Masha" "Ivanova" (Just $ BirthCertificate ("cd", 987654)) @?= child2

unit_greatestAncestor = do
  greatestAncestor child1 @?= person2
  greatestAncestor child2 @?= child2
  greatestAncestor person3 @?= person5

unit_ancestors = do
  ancestors 0 child1 @?= Set.fromList [child1]
  ancestors 1 child1 @?= Set.fromList [person1, person2]
  ancestors 2 child1 @?= Set.fromList []
  ancestors 0 person3 @?= Set.fromList [person3]
  ancestors 1 person3 @?= Set.fromList [person4]
  ancestors 2 person3 @?= Set.fromList [person5]
  ancestors 3 person3 @?= Set.fromList []

everyone :: Set.Set Person
everyone = Set.fromList [person1, person2, person3, person4, person5, child1, child2, child3]

unit_descendants = do
  descendants child1 everyone @?= Leaf child1 []
  descendants person1 everyone @?= Leaf person1 [(Leaf child1 []), (Leaf child3 [])]
  descendants person1 everyone @?= Leaf person1 [descendants child1 everyone, descendants child3 everyone]
  descendants person2 everyone @?= Leaf person2 [(Leaf child1 []), (Leaf child3 [])]

  descendants person3 everyone @?= Leaf person3 []
  descendants person4 everyone @?= Leaf person4 [Leaf person3 []]
  descendants person5 everyone @?= Leaf person5 [Leaf person4 [Leaf person3 []]]


