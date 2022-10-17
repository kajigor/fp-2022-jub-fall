module Test.Person where

import qualified Data.Set as Set
import Person
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

p1 = Person {firstName = "name_1", lastName = "lastname_1", formerLastNames = [], age = 100, parents = [], idNumber = Just (Passport (1111, 111111))}

p2 = Person {firstName = "name_2", lastName = "lastname_2", formerLastNames = [], age = 105, parents = [], idNumber = Just (Passport (1111, 111111))}

p3 = Person {firstName = "name_3", lastName = "lastname_3", formerLastNames = [], age = 0, parents = [Test.Person.p1, Test.Person.p2], idNumber = Just (Passport (1111, 111111))}

p4 = Person {firstName = "name_4", lastName = "lastname_4", formerLastNames = [], age = 0, parents = [Test.Person.p1, Test.Person.p2], idNumber = Just (Passport (1111, 111111))}

p5 = Person {firstName = "name_5", lastName = "lastname_5", formerLastNames = [], age = 0, parents = [Test.Person.p1, Test.Person.p2], idNumber = Just (Passport (1111, 111111))}

p6 = Person {firstName = "name_6", lastName = "lastname_6", formerLastNames = [], age = 107, parents = [], idNumber = Just (Passport (1111, 111111))}

p7 = Person {firstName = "name_7", lastName = "lastname_7", formerLastNames = [], age = 0, parents = [Test.Person.p6], idNumber = Just (Passport (1111, 111111))}

p8 = Person {firstName = "name_8", lastName = "lastname_8", formerLastNames = [], age = 0, parents = [Test.Person.p6], idNumber = Just (Passport (1111, 111111))}

p9 = Person {firstName = "name_9", lastName = "lastname_9", formerLastNames = [], age = 0, parents = [Test.Person.p4, Test.Person.p7], idNumber = Just (Passport (1111, 111111))}

p10 = Person {firstName = "name_10", lastName = "lastname_10", formerLastNames = [], age = 0, parents = [Test.Person.p5, Test.Person.p8], idNumber = Just (Passport (1111, 111111))}

p11 = Person {firstName = "name_11", lastName = "lastname_11", formerLastNames = [], age = 0, parents = [Test.Person.p5, Test.Person.p8], idNumber = Just (Passport (1111, 111111))}

p12 = Person {firstName = "name_12", lastName = "lastname_12", formerLastNames = [], age = 0, parents = [Test.Person.p18, Test.Person.p19], idNumber = Just (Passport (1111, 111111))}

p13 = Person {firstName = "name_13", lastName = "lastname_13", formerLastNames = [], age = 0, parents = [Test.Person.p11, Test.Person.p12], idNumber = Just (Passport (1111, 111111))}

p14 = Person {firstName = "name_14", lastName = "lastname_14", formerLastNames = [], age = 0, parents = [Test.Person.p20], idNumber = Just (Passport (1111, 111111))}

p15 = Person {firstName = "name_15", lastName = "lastname_15", formerLastNames = [], age = 0, parents = [Test.Person.p9, Test.Person.p14], idNumber = Just (Passport (1111, 111111))}

p16 = Person {firstName = "name_16", lastName = "lastname_16", formerLastNames = [], age = 0, parents = [Test.Person.p9, Test.Person.p14], idNumber = Just (Passport (1111, 111111))}

p17 = Person {firstName = "name_17", lastName = "lastname_17", formerLastNames = [], age = 0, parents = [Test.Person.p9, Test.Person.p14], idNumber = Just (Passport (1111, 111111))}

p18 = Person {firstName = "name_18", lastName = "lastname_18", formerLastNames = [], age = 102, parents = [], idNumber = Just (Passport (1111, 111111))}

p19 = Person {firstName = "name_19", lastName = "lastname_19", formerLastNames = [], age = 103, parents = [], idNumber = Just (Passport (1111, 111111))}

p20 = Person {firstName = "name_20", lastName = "lastname_20", formerLastNames = [], age = 104, parents = [Test.Person.p21], idNumber = Just (Passport (1111, 111111))}

p21 = Person {firstName = "name_21", lastName = "lastname_21", formerLastNames = [], age = 115, parents = [], idNumber = Just (Passport (1111, 111111))}

-- ┌────────────────────────────────────────────────────┐
-- │                                                    │
-- │  p21    p1──┬──p2         p6                       │
-- │   |         │              │                       │
-- │   |         │              │                       │
-- │   ▼         ▼              ▼                       │
-- │   p20    p3,p4,p5──────┬─p8,p7        p18──┬─p19   │
-- │    │        │          │     │             │       │
-- │    │        │          │     │             │       │
-- │    │        └─┬────────┼─────┘             │       │
-- │    │          │        │                   │       │
-- │    ▼          ▼        ▼                   ▼       │
-- │   p14───┬────p9     p10,p11──────┬────────p12      │
-- │         │                        │                 │
-- │         ▼                        ▼                 │
-- │    p15,p16,p17                  p13                │
-- │                                                    │
-- └────────────────────────────────────────────────────┘
unit_greatestAncestor = do
  greatestAncestor Test.Person.p1 @?= Test.Person.p1
  greatestAncestor Test.Person.p2 @?= Test.Person.p2
  greatestAncestor Test.Person.p3 @?= Test.Person.p2
  greatestAncestor Test.Person.p3 @?= Test.Person.p2
  greatestAncestor Test.Person.p4 @?= Test.Person.p2
  greatestAncestor Test.Person.p5 @?= Test.Person.p2
  greatestAncestor Test.Person.p6 @?= Test.Person.p6
  greatestAncestor Test.Person.p7 @?= Test.Person.p6
  greatestAncestor Test.Person.p8 @?= Test.Person.p6
  greatestAncestor Test.Person.p9 @?= Test.Person.p6
  greatestAncestor Test.Person.p10 @?= Test.Person.p6
  greatestAncestor Test.Person.p11 @?= Test.Person.p6
  greatestAncestor Test.Person.p12 @?= Test.Person.p19
  greatestAncestor Test.Person.p13 @?= Test.Person.p6
  greatestAncestor Test.Person.p14 @?= Test.Person.p21
  greatestAncestor Test.Person.p15 @?= Test.Person.p21
  greatestAncestor Test.Person.p16 @?= Test.Person.p21
  greatestAncestor Test.Person.p17 @?= Test.Person.p21
  greatestAncestor Test.Person.p18 @?= Test.Person.p18
  greatestAncestor Test.Person.p19 @?= Test.Person.p19
  greatestAncestor Test.Person.p20 @?= Test.Person.p21
  greatestAncestor Test.Person.p21 @?= Test.Person.p21

unit_ancestors = do
  ancestors 1 Test.Person.p15 @?= Set.fromList [Test.Person.p9, Test.Person.p14]
  ancestors 2 Test.Person.p15 @?= Set.fromList [Test.Person.p4, Test.Person.p7, Test.Person.p20]
  ancestors 3 Test.Person.p15 @?= Set.fromList [Test.Person.p1, Test.Person.p2, Test.Person.p6, Test.Person.p21]
  ancestors 4 Test.Person.p15 @?= Set.empty
  ancestors 1 Test.Person.p13 @?= Set.fromList [Test.Person.p11, Test.Person.p12]
  ancestors 2 Test.Person.p13 @?= Set.fromList [Test.Person.p5, Test.Person.p8, Test.Person.p18, Test.Person.p19]

unit_descedants = do
  let st = Set.fromList [Test.Person.p1, Test.Person.p2, Test.Person.p3, Test.Person.p4, Test.Person.p5, Test.Person.p6, Test.Person.p7, Test.Person.p8, Test.Person.p9, Test.Person.p10, Test.Person.p11, Test.Person.p12, Test.Person.p13, Test.Person.p14, Test.Person.p15, Test.Person.p16, Test.Person.p17, Test.Person.p18, Test.Person.p19, Test.Person.p20, Test.Person.p21]

  descendants Test.Person.p1 st @?= Node Test.Person.p1 [Leaf Test.Person.p3, Node Test.Person.p4 [Node Test.Person.p9 [Leaf Test.Person.p15, Leaf Test.Person.p16, Leaf Test.Person.p17]], Node Test.Person.p5 [Leaf Test.Person.p10, Node Test.Person.p11 [Leaf Test.Person.p13]]]

  descendants Test.Person.p2 st @?= Node Test.Person.p2 [Leaf Test.Person.p3, Node Test.Person.p4 [Node Test.Person.p9 [Leaf Test.Person.p15, Leaf Test.Person.p16, Leaf Test.Person.p17]], Node Test.Person.p5 [Leaf Test.Person.p10, Node Test.Person.p11 [Leaf Test.Person.p13]]]

  descendants Test.Person.p3 st @?= Leaf Test.Person.p3

  descendants Test.Person.p4 st @?= Node Test.Person.p4 [Node Test.Person.p9 [Leaf Test.Person.p15, Leaf Test.Person.p16, Leaf Test.Person.p17]]

  descendants Test.Person.p6 st @?= Node Test.Person.p6 [Node Test.Person.p7 [Node Test.Person.p9 [Leaf Test.Person.p15, Leaf Test.Person.p16, Leaf Test.Person.p17]], Node Test.Person.p8 [Leaf Test.Person.p10, Node Test.Person.p11 [Leaf Test.Person.p13]]]

  descendants Test.Person.p21 st @?= Node Test.Person.p21 [Node Test.Person.p20 [Node Test.Person.p14 [Leaf Test.Person.p15, Leaf Test.Person.p16, Leaf Test.Person.p17]]]
