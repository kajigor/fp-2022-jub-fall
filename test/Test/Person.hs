module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
    ( ancestors,
      createChild,
      descendants,
      greatestAncestor,
      Document(BirthCertificate, Passport),
      Person(..),
      Tree(TwoChildren, Leaf, OneChild) )
import qualified Data.Set as Set

person1 :: Person
person1 = Person {
    firstName = "Viktor",
    lastName = "Kharitontcev",
    formerLastNames = [],
    age = 12,
    idNumber = Just (Passport (12, 34)),
    parents = (Nothing, Nothing)
}
person2 :: Person
person2 = Person {
    firstName = "Svetlana",
    lastName = "Beglov",
    formerLastNames = [],
    age = 34,
    idNumber = Just (BirthCertificate ("I-AK", 78)),
    parents = (Nothing, Nothing)
}

person3 :: Person
person3 = Person {
    firstName = "3",
    lastName = "3",
    formerLastNames = [],
    age = 1,
    idNumber = Nothing,
    parents = (Just person1, Just person2)
}
person4 :: Person
person4 = Person {
    firstName = "4",
    lastName = "4",
    formerLastNames = [],
    age = 1,
    idNumber = Nothing,
    parents = (Just person1, Nothing)
}

person5 :: Person
person5 = Person {
    firstName = "5",
    lastName = "5",
    formerLastNames = [],
    age = 1,
    idNumber = Nothing,
    parents = (Just person4, Just person2)
}


unit_create_child = do
    let khb = createChild (Just person1) (Just person2) "Sergei"
    
    assertBool "wrong khb" (lastName khb == "Kharitontcev—Beglov")
    assertBool "wrong khb" (firstName khb == "Sergei")
    assertBool "wrong khb" (age khb == 0)
    assertBool "wrong khb" (parents khb == (Just person1, Just person2)) -- preserve order

    let child1 = createChild (Just khb) Nothing "Serg"
    let child2 = createChild Nothing (Just khb) "Serg"
    let child3 = createChild Nothing Nothing "lmao"

    assertBool "wrong 1" (lastName child1 == "Kharitontcev—Beglov")
    assertBool "wrong 1" (firstName child1 == "Serg")
    assertBool "wrong 1" (age child1 == 0)
    assertBool "wrong 1" (parents child1 == (Just khb, Nothing)) -- preserve order

    assertBool "wrong 2" (lastName child2 == "Kharitontcev—Beglov")
    assertBool "wrong 2" (firstName child2 == "Serg")
    assertBool "wrong 2" (age child2 == 0)
    assertBool "wrong 2" (parents child2 == (Nothing, Just khb)) -- preserve order

    assertBool "wrong 3" (lastName child3 == "N/A")
    assertBool "wrong 3" (firstName child3 == "lmao")
    assertBool "wrong 3" (age child3 == 0)
    assertBool "wrong 3" (parents child3 == (Nothing, Nothing)) -- preserve order

unit_greatest_ancestor = do
    greatestAncestor person1 @?= person1
    greatestAncestor person2 @?= person2
    greatestAncestor person3 @?= person2
    greatestAncestor person4 @?= person1
    greatestAncestor person5 @?= person1

unit_ancestors = do
    ancestors 0 person1 @?= Set.fromList [person1]
    ancestors 1 person3 @?= Set.fromList [person1, person2]
    ancestors 1 person4 @?= Set.fromList [person1]
    ancestors 1 person3 @?= Set.fromList [person1, person2]
    ancestors 1 person5 @?= Set.fromList [person4, person2]
    ancestors 2 person5 @?= Set.fromList [person1]

unit_descendants = do
    let t1 = Leaf person1
    let t2 = Leaf person2
    let t3 = TwoChildren person3 t1 t2
    let t4 = OneChild person4 t1
    let t5 = TwoChildren person5 t4 t2
    let res = [t1, t2, t3, t4, t5]
    let persons = [descendants person1, descendants person2, descendants person3, descendants person4, descendants person5]

    persons @?= res
