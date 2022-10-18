module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set

person1 =
  Person { firstName = "Arseny"
         , lastName = "Yusufov"
         , formerLastNames = []
         , age = 21
         , idNumber = Just (Passport (1234, 567890))
         , parents = Just (person2, person3)
  }

person2 =
  Person { firstName = "Some"
         , lastName = "Man"
         , formerLastNames = []
         , age = 46
         , idNumber = Just (Passport (1235, 567890))
         , parents = Nothing
  }

person3 =
  Person { firstName = "Some"
         , lastName = "Woman"
         , formerLastNames = []
         , age = 47
         , idNumber = Just (Passport (1236, 567890))
         , parents = Just (person8, person9)
  }

person4 =
  Person { firstName = "Marshall"
         , lastName = "Mathers"
         , formerLastNames = []
         , age = 50
         , idNumber = Just (Passport (7777, 567890))
         , parents = Nothing
  }

person5 =
  Person { firstName = "Hailie"
         , lastName = "Jade"
         , formerLastNames = []
         , age = 26
         , idNumber = Just (Passport (1236, 567890))
         , parents = Just (person4, person6)
  }

person6 =
  Person { firstName = "Kim"
         , lastName = "Jade"
         , formerLastNames = []
         , age = 47
         , idNumber = Just (Passport (1236, 567890))
         , parents = Nothing
  }

person7 =
  Person { firstName = "Zakhar"
         , lastName = "Yusufov"
         , formerLastNames = []
         , age = 47
         , idNumber = Just (Passport (1236, 567890))
         , parents = Just (person2, person3)
  }

person8 =
  Person { firstName = "Vladimir"
         , lastName = "Rabinovich"
         , formerLastNames = []
         , age = 72
         , idNumber = Just (Passport (1236, 567890))
         , parents = Nothing
  }

person9 =
  Person { firstName = "Anna"
         , lastName = "Girshfeld"
         , formerLastNames = []
         , age = 68
         , idNumber = Just (Passport (1236, 567890))
         , parents = Nothing
  }

unit_createChild = do
  let child = createChild person2 person3 ("New", "Brother", CertOfBirth ("lala", 12))
  firstName child @?= "New"
  lastName child @?= "Brother"
  age child @?= 0
  parents child @?= Just (person2, person3)
  idNumber child @?= Just (CertOfBirth ("lala", 12))
  let child2 = createChild person2 person3 ("Another", "Brother", Passport (1, 2)) -- not right, CertOfBirth should be provided
  firstName child2 @?= "Another"
  lastName child2 @?= "Brother"
  age child2 @?= 0
  parents child2 @?= Just (person2, person3)
  idNumber child2 @?= Nothing
  
unit_greatestAncestor = do
  greatestAncestor person1 @?= person8
  greatestAncestor person3 @?= person8
  greatestAncestor person2 @?= person2
  greatestAncestor person7 @?= person8

unit_uncestors = do
  ancestors 0 person1 @?= Set.singleton person1
  ancestors 1 person1 @?= Set.fromList [person2, person3]
  ancestors 2 person1 @?= Set.fromList [person8, person9]
  ancestors 2 person1 @?= ancestors 2 person7
  ancestors 1 person5 @?= Set.fromList [person4, person6]

unit_descendants = do
  let treeFor8 = Node person8 (Set.fromList [Node person3 (Set.fromList [Node person1 Set.empty, Node person7 Set.empty])])
  treeFor8 @?= descendants person8 (Set.fromList [person1, person2, person3, person4, person5, person6, person7, person8, person9])
  let treeFor4 = Node person4 (Set.singleton (Node person5 (Set.empty)))
  treeFor4 @?= descendants person4 (Set.fromList [person1, person2, person3, person4, person5, person6, person7, person8, person9])
