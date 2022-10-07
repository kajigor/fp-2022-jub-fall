module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set

person1 :: Person
person1 = Person {
    firstName = "1",
    lastName = "1",
    formerLastNames = [],
    age = 20,
    idNumber = Nothing,
    parents = (Nothing, Nothing)
}

person2 :: Person
person2 = Person {
    firstName = "2",
    lastName = "2",
    formerLastNames = [],
    age = 21,
    idNumber = Just $ Passport 1 2,
    parents = (Nothing, Nothing)
}

person3 :: Person
person3 = Person {
    firstName = "3",
    lastName = "3",
    formerLastNames = [],
    age = 22,
    idNumber = Nothing,
    parents = (Nothing, Nothing)
}

person4 :: Person
person4 = Person {
    firstName = "4",
    lastName = "4",
    formerLastNames = [],
    age = 21,
    idNumber = Nothing,
    parents = (Just person1, Just person2)
}

person5 :: Person
person5 = Person {
    firstName = "5",
    lastName = "5",
    formerLastNames = [],
    age = 19,
    idNumber = Nothing,
    parents = (Just person3, Nothing)
}

person6 :: Person
person6 = Person {
    firstName = "6",
    lastName = "6",
    formerLastNames = [],
    age = 18,
    idNumber = Nothing,
    parents = (Just person4, Just person5)
}

-- 1 - 4 - 6
--   /    /
--  2    /
-- 3 -  5


unit_create_child = do
    let child1 = createChild (Nothing, Nothing)
    let child2 = createChild (Just person1, Nothing)
    let child3 = createChild (Nothing, Just person2)
    let child4 = createChild (Just person1, Just person2)

    assertBool "wrong child" (age child1 == 0 && formerLastNames child1 == [] && parents child1 == (Nothing, Nothing))
    assertBool "wrong child" (age child2 == 0 && formerLastNames child2 == [] && parents child2 == (Just person1, Nothing) && lastName child2 == "1")
    assertBool "wrong child" (age child3 == 0 && formerLastNames child3 == [] && parents child3 == (Nothing, Just person2) && lastName child3 == "2")
    assertBool "wrong child" (age child4 == 0 && formerLastNames child4 == [] && parents child4 == (Just person1, Just person2) && lastName child4 == "1")

unit_greatest_ancestor = do
    greatestAncestor person6 @?= person3
    greatestAncestor person5 @?= person3
    greatestAncestor person4 @?= person2
    greatestAncestor person3 @?= person3
    greatestAncestor person2 @?= person2
    greatestAncestor person1 @?= person1

unit_ancestors = do
    ancestors 2 person6 @?= Set.fromList [person1, person2, person3]
    ancestors 1 person6 @?= Set.fromList [person4, person5]
    ancestors 0 person6 @?= Set.fromList [person6]

unit_descendants = do
    let tree1 = Leaf person1
    let tree2 = Leaf person2
    let tree3 = Leaf person3
    let tree4 = TwoChildren person4 tree1 tree2
    let tree5 = OneChild person5 tree3
    let tree6 = TwoChildren person6 tree4 tree5

    descendants person1 @?= tree1
    descendants person2 @?= tree2
    descendants person3 @?= tree3
    descendants person4 @?= tree4
    descendants person5 @?= tree5
    descendants person6 @?= tree6