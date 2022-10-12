module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
    ( ancestors,
      createChild,
      descendants,
      greatestAncestor,
      Document(Passport, BirthId),
      Person(..),
      Tree(Node) )

import qualified Data.Set as Set

person1 :: Person
person1 = Person {
    firstName = "Biba",
    lastName = "Ivanov",
    formerLastNames = [],
    age = 21,
    idNumber = Just (Passport 12 34),
    parents = (Nothing, Nothing)
}
person2 :: Person
person2 = Person {
    firstName = "Lina",
    lastName = "Wolf",
    formerLastNames = [],
    age = 25,
    idNumber = Just (Passport 67 89),
    parents = (Nothing, Nothing)
}

person3 :: Person
person3 = Person {
    firstName = "person3",
    lastName = "person3",
    formerLastNames = [],
    age = 12,
    idNumber = Nothing,
    parents = (Nothing, Nothing)
}
person4 :: Person
person4 = Person {
    firstName = "person4",
    lastName = "person4",
    formerLastNames = [],
    age = 0,
    idNumber = Nothing,
    parents = (Just person1, Just person2)
}

person5 :: Person
person5 = Person {
    firstName = "person5",
    lastName = "person5",
    formerLastNames = [],
    age = 0,
    idNumber = Nothing,
    parents = (Just person3, Just person4)
}


unit_create_child = do
    let clone4 = createChild "person4" "person4" (Just person1, Just person2)

    assertBool "wrong clone4" (clone4 == person4)

    let child1 = createChild "a" "b" (Just person3, Nothing)
    let child2 = createChild "a" "b" (Nothing, Just person3)
    let child3 = createChild "a" "b" (Nothing, Nothing)

    assertBool "wrong child1" ((firstName child1, lastName child1, parents child1) == ("a", "b", (Just person3, Nothing)))
    assertBool "wrong child2" ((firstName child2, lastName child2, parents child2) == ("a", "b", (Nothing, Just person3)))
    assertBool "wrong child3" ((firstName child3, lastName child3, parents child3) == ("a", "b", (Nothing, Nothing)))

unit_greatest_ancestor = do
    assertBool "wrong greatest1" (greatestAncestor person1 == person1)
    assertBool "wrong greatest2" (greatestAncestor person2 == person2)
    assertBool "wrong greatest3" (greatestAncestor person3 == person3)
    assertBool "wrong greatest4" (greatestAncestor person4 == person2)
    assertBool "wrong greatest5" (greatestAncestor person5 == person2)

unit_ancestors = do
    assertBool "wrong anc 0 1" (ancestors 0 person1 == Set.fromList [person1])
    assertBool "wrong anc 1 1" (ancestors 1 person1 == Set.fromList [])
    assertBool "wrong anc 1 4" (ancestors 1 person4 == Set.fromList [person1, person2])
    assertBool "wrong anc 1 5" (ancestors 1 person5 == Set.fromList [person3, person4])
    assertBool "wrong anc 2 5" (ancestors 2 person5 == Set.fromList [person1, person2])

unit_descendants = do
    let allPersons = Set.fromList [person1, person2, person3, person4, person5]

    assertBool "wrong des 5" (descendants allPersons person5 == Node person5 [])
    assertBool "wrong des 4" (descendants allPersons person4 == Node person4 [Node person5 []])
    assertBool "wrong des 3" (descendants allPersons person3 == Node person3 [Node person5 []])
    assertBool "wrong des 2" (descendants allPersons person2 == Node person2 [Node person4 [Node person5 []]])
    assertBool "wrong des 1" (descendants allPersons person1 == Node person1 [Node person4 [Node person5 []]])