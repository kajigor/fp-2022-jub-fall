module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set
import Data.Maybe
person1 :: Person
person1 =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = Just $ Passport (1234, 567890) 
         , parents = (Just person2, Just person3)}

person5 :: Person
person5 =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 20
         , idNumber = Just $ Passport (1234, 567890)
         , parents = (Just person1, Just person3) }

person6 :: Person
person6 =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 10
         , idNumber = Just $ Passport (1234, 567890)
         ,parents = (Just person1, Nothing) }

person2 :: Person
person2 =
  Person { firstName = "Ann"
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
         , parents = (Nothing, Nothing) }

person4 :: Person
person4 =
  Person { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 3
         , idNumber = Just $ Passport (1111, 111111)
         , parents = (Just person1, Just person5) }


child12 :: Person
child12 =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 0
         , idNumber = Just (BirthCert ("IIIP", 0))
         , parents = (Just person1, Just person2)}

child1 :: Person
child1 =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 0
         , idNumber = Just (BirthCert ("IIIP", 0))
         , parents = (Just person1, Nothing)}

child2 :: Person
child2 =
  Person { firstName = "Ann"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 0
         , idNumber = Just (BirthCert ("IIIP", 0))
         , parents = (Nothing, Just person2)}

orphan :: Person
orphan =
  Person { firstName = "orphan"
         , lastName = "orphan"
         , formerLastNames = []
         , age = 0
         , idNumber = Just (BirthCert ("IIIP", 0))
         , parents = (Nothing, Nothing)}

unit_createChild = do
  createChild (Just person1, Just person2) @?= child12
  createChild (Just person1, Nothing) @?= child1
  createChild (Nothing, Just person2) @?= child2
  createChild (Nothing, Nothing) @?= orphan

unit_greatestAncestors = do
  greatestAncestor person1 @?= person2
  greatestAncestor person3 @?= person3
  greatestAncestor person2 @?= person2
  greatestAncestor person5 @?= person2
  greatestAncestor person6 @?= person2

unit_ancestors = do
  ancestors 0 person1 @?= Set.singleton person1
  ancestors 1 person2 @?= Set.empty
  ancestors 2 person4 @?= Set.fromList [person1, person2, person3]
  ancestors 1 child1 @?= Set.singleton person1

unit_descendants = do
  let p2 = Terminal person2
  let p3 = Terminal person3
  let p1 = Node2 person1 p2 p3
  let p5 = Node2 person5 p1 p3
  descendants person1 @?= p1
  descendants person3 @?= p3
  descendants person4 @?= Node2 person4 p1 p5
  descendants person6 @?= Node1 person6 p1






