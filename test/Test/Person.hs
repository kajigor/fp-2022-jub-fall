module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set


person0::Person 
person0 = Person {
    firstName = "Juliia",
    lastName = "Vasileva",
    formerLastNames = ["Raksina", "Kardashian"],
    age = 29,
    idNumber = Just (Passport (1420, 891002)),
    parents = (Nothing, Nothing)
}

person1::Person 
person1 = Person {
    firstName = "Anasteisha",
    lastName = "Vasileva-Raksina",
    formerLastNames = ["Vasilva"],
    age = 30,
    idNumber = Just (Passport (1420, 891002)),
    parents = (Nothing, Nothing)
}

person2::Person 
person2 = Person {
    firstName = "Noize",
    lastName = "Kardashian",
    formerLastNames = [],
    age = 0,
    idNumber = Nothing,
    parents = (Nothing, Nothing)
}

person3::Person 
person3 = Person {
    firstName = "Petr",
    lastName = "Raksin-Kardashian",
    formerLastNames = [],
    age = 13,
    idNumber = (Just (BirthCertificateeId (5, "ab", 000123))),
    parents = (Just person0, Just person2)
}


person4::Person 
person4 = Person {
    firstName = "Danya",
    lastName = "Vasilev",
    formerLastNames = [],
    age = 0,
    idNumber = Nothing,
    parents = (Just person0, Just person1) 
}

person5::Person 
person5 = Person {
    firstName = "Author",
    lastName =  "WTF",
    formerLastNames = [],
    age = 0,
    idNumber = Nothing,
    parents = (Just person3, Nothing)
}

unit_create_child = do 
    let author = createChild (Nothing) (Just person3) "Author" "WTF"
    person5 @?= author
    let daniil = createChild (Just person0) (Just person1) "Danya" "Vasilev"
    person4 @?= daniil
    let noize_mc = createChild (Nothing) (Nothing) "Noize" "Kardashian"
    person2 @?= noize_mc

unit_greatest_ancestor = do
    greatestAncestor person0 @?= person0
    greatestAncestor person1 @?= person1
    greatestAncestor person2 @?= person2
    greatestAncestor person3 @?= person0
    greatestAncestor person4 @?= person1
    greatestAncestor person5 @?= person0

unit_anceestors = do
    ancestors (-1) person0 @?= Set.empty
    ancestors 0 person0 @?= Set.fromList [person0] 
    ancestors 1 person0 @?= Set.empty
    ancestors 1 person3 @?= Set.fromList [person0, person2]
    ancestors 1 person5 @?= Set.fromList [person3]
    ancestors 2 person5 @?= Set.fromList [person0, person2]


unit_descendants = do
    let allPeople = Set.fromList [person0, person1, person2, person3, person4, person5]
    descendants allPeople person5  @?= Leaf person5
    descendants allPeople person3  @?= Node person3 ( Set.fromList [Leaf person5] )
    descendants allPeople person2  @?= Node person2 ( Set.fromList [Node person3 (Set.fromList [Leaf person5])] )