module Test.Person where
import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set


person1 :: Person
person1 =
  Person { firstName = "Father"
         , lastName = "Papa"
         , formerLastNames = []
         , age = 29
         , idNumber = Just (Passport (123456, 123456))
         , parents = (Nothing, Nothing) }

person2 :: Person
person2 =
  Person { firstName = "Mother"
         , lastName = "Mama"
         , formerLastNames = []
         , age = 30
         , idNumber = Just (Passport (1234, 1234))
         , parents = (Nothing, Nothing) }


person12child :: Person
person12child =
  Person { firstName = "Natasha"
         , lastName = "Papa"
         , formerLastNames = []
         , age = 0
         , idNumber = Nothing
         , parents = (Just person1, Just person2) }

childNoParents :: Person
childNoParents =
  Person { firstName = "Masha"
         , lastName = "Sirota"
         , formerLastNames = []
         , age = 0
         , idNumber = Nothing
         , parents = (Nothing, Nothing) }


person3 :: Person
person3 =
  Person { firstName = "Oleg"
         , lastName = "Neoleg"
         , formerLastNames = []
         , age = 14
         , idNumber = Just (Passport (1234, 1234))
         , parents = (Nothing, Just person1) }

person4 :: Person
person4 =
  Person { firstName = "Sasha"
         , lastName = "Polyak"
         , formerLastNames = []
         , age = 14
         , idNumber = Just (Passport (1234, 1234))
         , parents = (Nothing, Nothing) }

unit_createChild = do
  let child' = createChild (Just person1, Just person2) "Natasha"
  child' @?= person12child
  let childNoParents' = createChild (Nothing, Nothing) "Masha"
  childNoParents' @?= childNoParents

people = Set.fromList [person1, person2, person3, person4,
                       person12child, childNoParents]

unit_descendants = do
  descendants people person3 @?= Node person3 Set.empty