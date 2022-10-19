module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set


person1 :: Person
person1 =
  Person { firstName = "N"
         , lastName = "N"
         , formerLastNames = []
         , age = 29
         , idNumber = Passport (1234, 567890)
         , parents = (Nothing, Nothing) }

person2 :: Person
person2 =
  Person { firstName = "M"
         , lastName = "M"
         , formerLastNames = []
         , age = 30
         , idNumber = Passport (1234, 567891)
         , parents = (Nothing, Nothing) }


person12child :: Person
person12child =
  Person { firstName = "One"
         , lastName = "N"
         , formerLastNames = []
         , age = 0
         , idNumber = BirthCert ("ao", 123456)
         , parents = (Just person2, Just person1) }

childNoParents :: Person
childNoParents =
  Person { firstName = "No"
         , lastName = "Parents"
         , formerLastNames = []
         , age = 0
         , idNumber = BirthCert ("ao", 123456)
         , parents = (Nothing, Nothing) }


person3 :: Person
person3 =
  Person { firstName = "U"
         , lastName = "N"
         , formerLastNames = []
         , age = 14
         , idNumber = Passport (1234, 567892)
         , parents = (Nothing, Just person2) }

person4 :: Person
person4 =
  Person { firstName = "A"
         , lastName = "B"
         , formerLastNames = []
         , age = 14
         , idNumber = Passport (1234, 567892)
         , parents = (Nothing, Nothing) }

person5 :: Person
person5 =
  Person { firstName = "C"
         , lastName = "C"
         , formerLastNames = []
         , age = 14
         , idNumber = Passport (1234, 567892)
         , parents = (Nothing, Nothing) }


unit_createChild = do
  let child' = createChild "One" (lastName person1) (Just person2, Just person1)
  child' @?= person12child
  let childNoParents' = createChild "No" "Parents" (Nothing, Nothing)
  childNoParents' @?= childNoParents


person4child = createChild "O" "B" (Just person4, Nothing)
person4childchild = createChild "P" "B" (Just person4child, Just person5)
person4childchildchild = createChild "G" "B" (Nothing, Just person4childchild)
finalchild = createChild "Z" "B" (Just person12child, Just person4childchildchild)

unit_greatestAncestor = do
  greatestAncestor person1 @?= person1
  greatestAncestor person4child @?= person4
  greatestAncestor finalchild @?=  person4
  greatestAncestor finalchild @?= greatestAncestor person4childchildchild
  greatestAncestor person3 @?= person2
  greatestAncestor person12child @?= person2


unit_ancestors = do
  ancestors 2 person1 @?= Set.empty
  ancestors 2 finalchild @?= Set.fromList [person1, person2, person4childchild]
  ancestors 3 finalchild @?= Set.fromList [person5, person4child]
  ancestors 4 finalchild @?= Set.singleton person4


unit_descendants = do
    let people = Set.fromList [person1, person2, person3, person4, person5,
                               person12child, person4child, person4childchild, person4childchildchild,
                               childNoParents, finalchild]

    let tree1 = Node finalchild Set.empty
    let tree2 = Node person12child (Set.fromList [tree1])
    let tree3 = Node person3 Set.empty
    let tree4 = Node person2 (Set.fromList [tree2, tree3])

    descendants people finalchild @?= Node finalchild Set.empty
    descendants people person2 @?= tree4
