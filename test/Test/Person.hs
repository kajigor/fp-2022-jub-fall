module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import Data.Set as Set

greatGrandMotherM :: Person
greatGrandMotherM = Person
    { firstName = "Galina",
      lastName = "Pamfilova",
      formerLastNames = [],
      age = 101,
      document = Just (Passport(3322, 555444)),
      parents = Nothing
    }

grandMotherM :: Person
grandMotherM = Person
    { firstName = "Claudia",
      lastName = "Sidorova",
      formerLastNames = [],
      age = 81,
      document = Just (Passport(2222, 555444)),
      parents = Just (greatGrandMotherM, greatGrandMotherM)
    }

grandFatherM :: Person
grandFatherM = Person
    { firstName = "Innokentii",
      lastName = "Sidorov",
      formerLastNames = [],
      age = 85,
      document = Just (Passport(3333, 666777)),
      parents = Nothing
    }

mother :: Person
mother = Person
    { firstName = "Anna",
      lastName = "Ivanova",
      formerLastNames = [],
      age = 51,
      document = Just (Passport(2222, 444333)),
      parents = Just (grandMotherM, grandFatherM)
    }

greatGrandMotherF :: Person
greatGrandMotherF = Person
    { firstName = "Eleanora",
      lastName = "Ivanova",
      formerLastNames = [],
      age = 99,
      document = Just (Passport(2233, 444555)),
      parents = Nothing
    }

greatGrandFatherF :: Person
greatGrandFatherF = Person
    { firstName = "Zahar",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 105,
      document = Just (Passport(1233, 144555)),
      parents = Nothing
    }

grandMotherF :: Person
grandMotherF = Person
    { firstName = "Anastasija",
      lastName = "Ivanova",
      formerLastNames = [],
      age = 78,
      document = Just (Passport(2222, 999333)),
      parents = Just(greatGrandMotherF, greatGrandFatherF)
    }

grandFatherF :: Person
grandFatherF = Person
    { firstName = "Petr",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 88,
      document = Just (Passport(2222, 444888)),
      parents = Nothing
    }

father :: Person
father = Person
    { firstName = "Ivan",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 59,
      document = Just (Passport(3333, 444333)),
      parents = Just(grandMotherF, grandFatherF)
    }

personWithoutParents :: Person
personWithoutParents =
  Person
    { firstName = "Mark",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 21,
      document = Just (Passport(1111, 444333)),
      parents = Nothing
    }

personWithParents :: Person
personWithParents =
  Person
    { firstName = "Mark",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 21,
      document = Just (Passport(1111, 444333)),
      parents = Just(mother, father)
    }

child1 :: Person
child1 =
  Person
    { firstName = "Mark II",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 1,
      document = Just (BirthCertificate 1111),
      parents = Just(personWithoutParents, personWithoutParents)
    }

child2 :: Person
child2 =
  Person
    { firstName = "Mark III",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 2,
      document = Just (BirthCertificate 2222),
      parents = Just(personWithoutParents, personWithoutParents)
    }

unit_createChild = do
    personWithParents @?= createChild (mother, father) personWithoutParents

unit_ancestors = do
    Set.empty @?= ancestors 0 personWithoutParents
    Set.empty @?= ancestors 3 personWithoutParents
    Set.fromList [mother, father] @?= ancestors 0 personWithParents
    Set.fromList [grandMotherM, grandFatherM, grandMotherF, grandFatherF] @?= ancestors 1 personWithParents
    Set.fromList [greatGrandMotherM, greatGrandMotherF, greatGrandFatherF] @?= ancestors 2 personWithParents
    Set.empty @?= ancestors 3 personWithParents

unit_greatestAncestor = do
    personWithoutParents @?= greatestAncestor personWithoutParents
    greatGrandFatherF @?= greatestAncestor personWithParents

-- unit_descendants = do
    -- Tree personWithoutParents Set.empty @?= descendants personWithoutParents Set.empty
    -- Tree personWithoutParents (Set.fromList [Tree child1]) @?= descendants personWithoutParents (Set.fromList [child1])
    -- Descendant (Set.fromList [child1, child2]) @?= descendants personWithoutParents (Set.fromList [child1, child2])
    -- Descendant (Set.fromList [personWithParents]) @?= descendants mother (Set.fromList [personWithParents, child1, child2])
    -- Descendant (Set.fromList [mother, personWithParents]) @?= descendants grandMotherM (Set.fromList [mother, personWithParents, child1, child2])

    