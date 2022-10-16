module Test.Person where

import qualified Data.Set as Set
import Person
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

p1 :: Person
p1 =
  Person
    { personId = 1,
      firstName = "p1_firstName",
      lastName = "p1_lastName",
      formerLastNames = [],
      age = 1001,
      idDoc = Just (Passport_ (Passport (0001, 000001))),
      parents = Set.empty
    }

p2 :: Person
p2 =
  Person
    { personId = 2,
      firstName = "p2_firstName",
      lastName = "p2_lastName",
      formerLastNames = ["p2_former"],
      age = 1002,
      idDoc = Just (Passport_ (Passport (0001, 000002))),
      parents = Set.empty
    }

p3 :: Person
p3 =
  Person
    { personId = 3,
      firstName = "p3_firstName",
      lastName = "p3_lastName",
      formerLastNames = [],
      age = 1002,
      idDoc = Just (Passport_ (Passport (0001, 000003))),
      parents = Set.empty
    }

p4 :: Person
p4 =
  Person
    { personId = 4,
      firstName = "p4_firstName",
      lastName = "p4_lastName",
      formerLastNames = [],
      age = 101,
      idDoc = Just (Passport_ (Passport (0002, 000004))),
      parents = Set.fromList [p1, p2]
    }

p5 :: Person
p5 =
  Person
    { personId = 5,
      firstName = "p5_firstName",
      lastName = "p5_lastName",
      formerLastNames = [],
      age = 102,
      idDoc = Just (Passport_ (Passport (0002, 000005))),
      parents = Set.fromList [p1, p2]
    }

p6 :: Person
p6 =
  Person
    { personId = 6,
      firstName = "p6_firstName",
      lastName = "p6_lastName",
      formerLastNames = [],
      age = 103,
      idDoc = Just (Passport_ (Passport (0002, 000006))),
      parents = Set.fromList [p1, p2, p3]
    }

p7 :: Person
p7 =
  Person
    { personId = 7,
      firstName = "p7_firstName",
      lastName = "p7_lastName",
      formerLastNames = [],
      age = 104,
      idDoc = Just (Passport_ (Passport (0002, 000007))),
      parents = Set.fromList []
    }

p8 :: Person
p8 =
  Person
    { personId = 8,
      firstName = "p8_firstName",
      lastName = "p8_lastName",
      formerLastNames = [],
      age = 10,
      idDoc = Just (BirthCert_ (BirthCert ("AAA", 000008))),
      parents = Set.fromList [p4]
    }

p9 :: Person
p9 =
  Person
    { personId = 9,
      firstName = "p9_firstName",
      lastName = "p9_lastName",
      formerLastNames = [],
      age = 11,
      idDoc = Just (BirthCert_ (BirthCert ("AAA", 000009))),
      parents = Set.fromList [p4]
    }

p10 :: Person
p10 =
  Person
    { personId = 10,
      firstName = "p10_firstName",
      lastName = "p10_lastName",
      formerLastNames = [],
      age = 12,
      idDoc = Just (BirthCert_ (BirthCert ("AAA", 000010))),
      parents = Set.fromList [p5]
    }

p11 :: Person
p11 =
  Person
    { personId = 11,
      firstName = "p11_firstName",
      lastName = "p11_lastName",
      formerLastNames = [],
      age = 13,
      idDoc = Just (BirthCert_ (BirthCert ("AAA", 000011))),
      parents = Set.fromList [p6, p7]
    }

unit_createChild = do
  createChild (personId p1) (firstName p1) (lastName p1) (age p1) (idDoc p1) (parents p1) @?= Just p1
  createChild (personId p6) (firstName p6) (lastName p6) (age p6) (idDoc p6) (parents p6) @?= Just p6
  createChild (personId p8) (firstName p8) (lastName p8) (age p8) (idDoc p8) (parents p8) @?= Just p8
  createChild (personId p4) (firstName p4) (lastName p4) (age p4) (idDoc p4) (Set.fromList [p1, p11]) @?= Nothing

unit_greatestAncestor = do
  greatestAncestor p1 @?= p1
  greatestAncestor p2 @?= p2
  greatestAncestor p3 @?= p3
  greatestAncestor p7 @?= p7
  greatestAncestor p4 @?= p2
  greatestAncestor p5 @?= p2
  assertBool "need one of oldest ancestor" (greatestAncestor p6 == p2 || greatestAncestor p6 == p3)
  greatestAncestor p8 @?= p2
  greatestAncestor p9 @?= p2
  greatestAncestor p10 @?= p2
  assertBool "need one of oldest ancestor" (greatestAncestor p11 == p2 || greatestAncestor p11 == p3)

unit_ancestors = do
  ancestors 1 p8 @?= parents p8
  ancestors 2 p8 @?= Set.fromList [p1, p2]
  ancestors 2 p11 @?= Set.fromList [p1, p2, p3]
  ancestors 1 p11 @?= parents p11
  ancestors 1 p6 @?= parents p6

  ancestors 0 p8 @?= Set.empty
  ancestors (-1) p8 @?= Set.empty
  ancestors 3 p8 @?= Set.empty
  ancestors 1 p1 @?= Set.empty
  ancestors 3 p7 @?= Set.empty

people = Set.fromList [p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11]

unit_descendants = do
  descendants people p8 @?= Node p8 []
  descendants people p9 @?= Node p9 []
  descendants people p10 @?= Node p10 []
  descendants people p11 @?= Node p11 []

  descendants people p4 @?= Node p4 [(descendants people p8), (descendants people p9)]
  descendants people p5 @?= Node p5 [(descendants people p10)]
  descendants people p6 @?= Node p6 [(descendants people p11)]
  descendants people p7 @?= Node p7 [(descendants people p11)]

  descendants people p1 @?= Node p1 [(descendants people p4), (descendants people p5), (descendants people p6)]
  descendants people p2 @?= Node p2 [(descendants people p4), (descendants people p5), (descendants people p6)]
  descendants people p3 @?= Node p3 [(descendants people p6)]
