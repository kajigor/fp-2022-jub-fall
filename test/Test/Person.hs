module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set

person1 :: Person
person1 = Person {
    firstName = "Oleg",
    lastName = "Makeev",
    formerLastNames = [],
    age = 29,
    document = Just (Passport (77, 9993299)),
    parents = (Nothing, Nothing)
}

person2 :: Person
person2 = Person {
    firstName = "Oleg",
    lastName = "Veshiy",
    formerLastNames = ["Vsevidyashiy"],
    age = 23,
    document = Just (Passport (77, 77777777)),
    parents = (Nothing, Nothing)
}

person3 :: Person
person3 = Person {
    firstName = "Machael",
    lastName = "Miringov",
    formerLastNames = [],
    age = 82,
    document = Just (Passport (99, 99999999)),
    parents = (Nothing, Nothing)
}

person4 :: Person
person4 = Person {
    firstName = "Timur",
    lastName = "Malanin",
    formerLastNames = ["Podkast"],
    age = 93,
    document = Just (Passport (10, 1010101010)),
    parents = (Just person1, Just person2)
}

person5 :: Person
person5 = Person {
    firstName = "Daniil",
    lastName = "Karol",
    formerLastNames = ["Buntar", "Meduzu--mne-v-rot"],
    age = 23,
    document = Just (Passport (123, 123123123)),
    parents = (Just person3, Nothing)
}

person6 :: Person
person6 = Person {
    firstName = "Peson",
    lastName = "Personovich",
    formerLastNames = [],
    age = 12,
    document = Just (Passport (232, 123123123)),
    parents = (Just person4, Just person5)
}

unit_create_child = do
    let child1 = createChild "1" "2" "SS" 13132 (Nothing, Nothing)
    let child2 = createChild "3" "4" "QS" 32132 (Just person1, Nothing)
    let child3 = createChild "5" "6" "FF" 22424 (Nothing, Just person2)
    let child4 = createChild "1" "2" "SS" 86868 (Just person1, Just person2)

    assertBool "wrong child" (age child1 == 0 && formerLastNames child1 == [] && parents child1 == (Nothing, Nothing))
    assertBool "wrong child" (age child2 == 0 && formerLastNames child2 == [] && parents child2 == (Just person1, Nothing))
    assertBool "wrong child" (age child3 == 0 && formerLastNames child3 == [] && parents child3 == (Nothing, Just person2))
    assertBool "wrong child" (age child4 == 0 && formerLastNames child4 == [] && parents child4 == (Just person1, Just person2))

unit_greatest_ancestor = do
    greatestAncestor person6 @?= person3
    greatestAncestor person5 @?= person3
    greatestAncestor person4 @?= person1
    greatestAncestor person3 @?= person3
    greatestAncestor person2 @?= person2
    greatestAncestor person1 @?= person1

unit_ancestors = do
    ancestors 2 person6 @?= Set.fromList [person1, person2, person3]
    ancestors 1 person6 @?= Set.fromList [person4, person5]
    ancestors 0 person6 @?= Set.fromList [person6]

unit_descendants = do
    let people = Set.fromList [person1, person2, person3, person4,
                       person5, person6]
    let tree1 = Node person6 Set.empty
    let tree2 = Node person4 (Set.fromList [tree1])
    let tree3 = Node person2 (Set.fromList [tree2])
    descendants people person6 @?= Node person6 Set.empty
    descendants people person2 @?= tree3
