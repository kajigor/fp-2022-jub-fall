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
         , parents = (Just person1, Just person2) }

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
         , parents = (Nothing, Just person1) }

person4 :: Person
person4 =
  Person { firstName = "A"
         , lastName = "B"
         , formerLastNames = []
         , age = 14
         , idNumber = Passport (1234, 567892)
         , parents = (Nothing, Nothing) }


unit_createChild = do
  let child' = createChild "One" (lastName person1) (Just person1, Just person2)
  child' @?= person12child
  let childNoParents' = createChild "No" "Parents" (Nothing, Nothing)
  childNoParents' @?= childNoParents


person4child = createChild "O" "B" (Just person4, Nothing)
person4childchild = createChild "P" "B" (Nothing, Just person4child)
person4childchildchild = createChild "G" "B" (Just person4childchild, Nothing)
finalchild = createChild "Z" "B" (Just person12child, Just person4childchild)

unit_greatestAncestor = do
  greatestAncestor person1 @?= person1
  greatestAncestor person4child @?= person4
  greatestAncestor finalchild @?=  person4
  greatestAncestor finalchild @?= greatestAncestor person4childchildchild


unit_ancestors = do
  ancestors 2 person1 @?= Set.empty
  ancestors 2 finalchild @?= Set.fromList [person1, person2, person4child]


people = Set.fromList [person1, person2, person3, person4,
                       person12child, childNoParents]

unit_descendants = do
  descendants people person3 @?= Node person3 Set.empty