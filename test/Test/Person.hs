module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import qualified Data.Set as Set
import Person
    ( ancestors,
      createChild,
      descendants,
      greatestAncestor,
      Document(BirthCertificate, Passport),
      Person(..),
      Tree(Leaf, Node) )

person1 :: Person
person1 = Person {
    firstName = "laba",
    lastName = "daba",
    formerLastNames = [],
    age = 16,
    idNumber = Just (BirthCertificate ("FGH", 322)),
    parents = (Nothing, Nothing)
}

person2 :: Person
person2 = Person {
    firstName = "jaja",
    lastName = "kaja",
    formerLastNames = [],
    age = 22,
    idNumber = Just (Passport (228, 43)),
    parents = (Nothing, Nothing)
}

person3 :: Person
person3 = Person {
    firstName = "qwer",
    lastName = "rewq",
    formerLastNames = [],
    age = 37,
    idNumber = Just (Passport (432, 432)),
    parents = (Just person1, Just person2)
}

person4 :: Person
person4 = Person {
    firstName = "asdf",
    lastName = "fdas",
    formerLastNames = [],
    age = 32,
    idNumber = Just (Passport (2345, 2345)),
    parents = (Just person3, Nothing)
}

person5 :: Person
person5 = Person {
    firstName = "zxcv",
    lastName = "vcxz",
    formerLastNames = [],
    age = 11,
    idNumber = Just (BirthCertificate ("SDFG", 78)),
    parents = (Just person2, Just person3)
}

person6::Person
person6 = Person {
    firstName = "tt",
    lastName =  "tt",
    formerLastNames = [],
    age = 0,
    idNumber = Nothing,
    parents = (Nothing, Nothing)
}

person7::Person
person7 = Person {
    firstName = "tt",
    lastName =  "tt",
    formerLastNames = [],
    age = 0,
    idNumber = Nothing,
    parents = (Nothing, Just person5)
}




unit_create_child = do
    let c1 = createChild (Just person1) (Just person2) "iurii" (lastName person1)
    let c2 = createChild (Just person2) Nothing "a" (lastName person2)
    let c3  = createChild Nothing (Just person3) "b" (lastName person3)
    let c4 = (createChild Nothing Nothing "c" "hristos")
    let aba = createChild (Nothing) (Nothing) "tt" "tt"
    assertBool "error c1" (lastName c1 == (lastName person1))
    assertBool "error c1" (firstName c1 == "iurii")
    assertBool "error c1" (age c1 == 0)
    assertBool "error c1" (parents c1 == (Just person1, Just person2))
    assertBool "error c2" (lastName c2 == (lastName person2))
    assertBool "error c2" (firstName c2 == "a")
    assertBool "error c2" (age c2 == 0)
    assertBool "error c2" (parents c2 == ((Just person2), Nothing))
    assertBool "error c3" (lastName c3 == (lastName person3))
    assertBool "error c3" (firstName c3 == "b")
    assertBool "error c3" (age c3 == 0)
    assertBool "error c3" (parents c3 == (Nothing, (Just person3)))
    assertBool "error c4" (lastName c4 == "hristos")
    assertBool "error c4" (firstName c4 == "c")
    assertBool "error c4" (age c4 == 0)
    assertBool "error c4" (parents c4 == (Nothing, Nothing))
    person6 @?= aba

unit_greatest_ancestor = do
    greatestAncestor person1 @?= person1
    greatestAncestor person2 @?= person2
    greatestAncestor person3 @?= person2
    greatestAncestor person4 @?= person2
    greatestAncestor person5 @?= person2

unit_ancestors = do
    ancestors (-1) person1 @?= Set.empty
    ancestors 0 person1 @?= Set.fromList [person1]
    ancestors 1 person3 @?= Set.fromList [person1, person2]
    ancestors 1 person4 @?= Set.fromList [person3]
    ancestors 1 person5 @?= Set.fromList [person2, person3]
    ancestors 2 person4 @?= Set.fromList [person1, person2]

p1 :: Person
p1 = Person {
    firstName = "laba",
    lastName = "daba",
    formerLastNames = [],
    age = 16,
    idNumber = Just (BirthCertificate ("FGH", 322)),
    parents = (Nothing, Nothing)
}

p2 :: Person
p2 = Person {
    firstName = "jaja",
    lastName = "kaja",
    formerLastNames = [],
    age = 22,
    idNumber = Just (Passport (228, 43)),
    parents = (Nothing, Nothing)
}

p3 :: Person
p3 = Person {
    firstName = "qwer",
    lastName = "rewq",
    formerLastNames = [],
    age = 37,
    idNumber = Just (Passport (432, 432)),
    parents = (Just p1, Just p2)
}

p4 :: Person
p4 = Person {
    firstName = "qwer",
    lastName = "rewq",
    formerLastNames = [],
    age = 37,
    idNumber = Just (Passport (432, 432)),
    parents = (Just p1, Nothing)
}

unit_descendants = do
    let all = Set.fromList [p1, p2, p3, p4]
    descendants p1 all @?= Node p1 [Leaf p4, Leaf p3]
    descendants p2 all @?= Node p2 [Leaf p3]
    descendants p3 all @?= Leaf p3