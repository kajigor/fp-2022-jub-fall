module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import qualified Data.Set as Set
import Data.Maybe
import Person

person1 :: Person
person1 =
    Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , document = Just (Passport (1234, 567890))
         , parents = (Just parent1, Just parent2)         
         }

parent1 :: Person
parent1 = 
  Person { firstName = "Alisa"
         , lastName = "Verbitskaia"
         , formerLastNames = ["Mikhailova"]
         , age = 52
         , document = Just (Passport (0111, 433222))
         , parents = (Just grandparent1, Nothing)         
         }

parent2 :: Person
parent2 = 
  Person { firstName = "Nikita"
         , lastName = "Verbitskii"
         , formerLastNames = []
         , age = 53
         , document = Just (Passport (0101, 433212))
         , parents = (Nothing, Nothing)         
         }

person2 :: Person
person2 = 
  Person { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 30
         , document = Just (Passport (1111, 121212))
         , parents = (Just parent3, Just parent4)         
         }

person3 :: Person
person3 = 
  Person { firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 30
         , document = Just (Passport (1111, 212121))
         , parents = (Just parent3, Just parent4)         
         }

child1 :: Person
child1 = 
 Person { firstName = "Arina"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 0
         , document = Nothing
         , parents = (Just person1, Just person2)         
         }

child2 :: Person
child2 = 
  Person { firstName = "Irina"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 0
         , document = Nothing
         , parents = (Just person3, Nothing)         
         }

child3 :: Person
child3 = 
  Person { firstName = "John"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 0
         , document = Nothing
         , parents = (Nothing, Nothing)         
         }

parent3 :: Person
parent3 = 
  Person { firstName = "Nina"
         , lastName = "Kozak"
         , formerLastNames = []
         , age = 55
         , document = Just (Passport (0011, 123433))
         , parents = (Nothing, Nothing)         
         }

parent4 :: Person
parent4 = 
  Person { firstName = "Vlad"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 55
         , document = Just (Passport (0033, 123444))
         , parents = (Nothing, Nothing)         
         }

grandparent1 :: Person
grandparent1 = 
  Person { firstName = "Andrei"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 75
         , document = Just (Passport (0000, 000000))
         , parents = (Nothing, Nothing)         
         }

unit_createChild = do
  createChild "Arina" "Ivanova" (Just person1, Just person2) @?= child1
  createChild "Irina" "Ivanova" (Nothing, Just person3) @?= child2
  createChild "Irina" "Ivanova" (Just person3, Nothing) @?= child2
  createChild "John" "Smith" (Nothing, Nothing) @?= child3


unit_greatestAncestor = do 
  greatestAncestor child1 @?= grandparent1
  greatestAncestor person1 @?= grandparent1
  greatestAncestor parent1 @?= grandparent1
  greatestAncestor grandparent1 @?= grandparent1

  let person2_greatestAncestor = greatestAncestor person2
  assertBool "True" ((person2_greatestAncestor == parent3) || (person2_greatestAncestor == parent4)) 

unit_ancestors = do
  ancestors 0 person2 @?= Set.singleton person2
  ancestors 2 child1 @?= Set.fromList [parent1, parent2, parent3, parent4]
  ancestors 1 child1 @?= Set.fromList [person1, person2]
  ancestors 1 person1 @?= Set.fromList [parent1, parent2]
  ancestors 1 grandparent1 @?= Set.empty

unit_descendants = do
    let people = Set.fromList [child1, child2, person1, person2, person3, parent1, parent2, parent3, parent4]
    let lastGeneration = Vertex child1 Set.empty

    descendants child1 people @?= lastGeneration

    descendants person1 people @?= Vertex person1 (Set.singleton lastGeneration)
    descendants person2 people @?= Vertex person2 (Set.singleton lastGeneration)

    descendants parent1 people @?= Vertex parent1 (Set.singleton (Vertex person1 (Set.singleton lastGeneration)))
    descendants parent3 people @?= 
        Vertex parent3 (Set.fromList 
            [Vertex person2 (Set.singleton lastGeneration), Vertex person3 (Set.singleton (Vertex child2 Set.empty))]
            )
 