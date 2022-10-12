module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set

person1 :: Person
person1 =
  Person { firstName = "Oleg"
         , lastName = "Makeev"
         , formerLastNames = []
         , age = 19
         , idNumber = Just (Passport (1234, 567890))
         , parents = Set.fromList [person3, person4]
         }

person2 =
  Person { firstName = "Ian"
         , lastName = "Curtis"
         , formerLastNames = []
         , age = 27
         , idNumber = Just (Passport (1234, 567890))
         , parents = Set.fromList [person3, person4]
         }

person3 =
  Person { firstName = "Daft"
         , lastName = "Punk"
         , formerLastNames = []
         , age = 40
         , idNumber = Just (Passport (1234, 567890))
         , parents = Set.empty
         }

person4 =
  Person { firstName = "Didi"
         , lastName = "Ramon"
         , formerLastNames = []
         , age = 31
         , idNumber = Just (Passport (1234, 567890))
         , parents = Set.fromList [person5]
         }

person5 =
  Person { firstName = "Jack"
         , lastName = "White"
         , formerLastNames = []
         , age = 44
         , idNumber = Just (Passport (1234, 567890))
         , parents = Set.fromList [person8, person7]
         }

person6 =
  Person { firstName = "John"
         , lastName = "Lennon"
         , formerLastNames = []
         , age = 90
         , idNumber = Just (Passport (1234, 567890))
         , parents = Set.fromList [person7]
         }


person7 =
  Person { firstName = "Keith"
         , lastName = "Haring"
         , formerLastNames = []
         , age = 39
         , idNumber = Just (Passport (1234, 567890))
         , parents = Set.fromList [person8, person9]
         }

person8 =
  Person { firstName = "John"
         , lastName = "Kennedy"
         , formerLastNames = []
         , age = 41
         , idNumber = Just (Passport (1234, 567890))
         , parents = Set.empty
         }

person9 =
  Person { firstName = "Jesus"
         , lastName = "Christ"
         , formerLastNames = []
         , age = 2022
         , idNumber = Just (Passport (1234, 567890))
         , parents = Set.empty
         }

child =
  Person { firstName = "Iggy"
         , lastName = "Pop"
         , formerLastNames = []
         , age = 22
         , idNumber = Just (Passport (1234, 567890))
         , parents = Set.fromList [youngerParent, olderParent]
         }

youngerParent =
  Person { firstName = "Andy"
         , lastName = "Warhol"
         , formerLastNames = []
         , age = 55
         , idNumber = Just (Passport (1234, 567890))
         , parents = Set.empty
         }

olderParent =
  Person { firstName = "Jim"
         , lastName = "Morrison"
         , formerLastNames = []
         , age = 99
         , idNumber = Just (Passport (1234, 567890))
         , parents = Set.empty
         }

unit_createChild = do
    let child = createChild (Set.fromList [person1, person2]) "blabla" "human" (Just (BirthCertificate ("abc", 123456)))
    firstName child @?= "blabla"
    lastName child @?= "human"
    formerLastNames child @?= []
    age child @?= 0
    idNumber child @?= Just (BirthCertificate ("abc", 123456))
    parents child @?= Set.fromList [person1, person2]

unit_ancestors = do
    ancestors 0 person1 @?= Set.singleton person1
    ancestors 1 person1 @?= Set.fromList [person3, person4]
    ancestors 2 person1 @?= Set.fromList [person5]
    ancestors 100 person1 @?= Set.empty

unit_greatestAncestor = do
    greatestAncestor person1 @?= person9
    greatestAncestor person9 @?= person9
    greatestAncestor child @?= olderParent

unit_descendants = do
    let humans = Set.fromList [person1, person2, person3, person4, person5, person6, person7, person8, person9]
    let descendants_for_person4 = Node person4 (Set.fromList [Node person2 Set.empty, Node person1 Set.empty])
    let descendants_for_person7 = Node person7 (Set.fromList [Node person5 (Set.singleton descendants_for_person4), Node person6 Set.empty])
    descendants person4 humans @?= descendants_for_person4