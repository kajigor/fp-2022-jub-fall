module Test.Person where

import Data.Set as Set
import Person
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))

greatGrandMotherM :: Person
greatGrandMotherM =
  Person
    { firstName = "Galina",
      lastName = "Pamfilova",
      formerLastNames = [],
      age = 101,
      document = Just (Passport (3322, 555444)),
      parents = Nothing
    }

grandMotherM :: Person
grandMotherM =
  Person
    { firstName = "Claudia",
      lastName = "Sidorova",
      formerLastNames = [],
      age = 81,
      document = Just (Passport (2222, 555444)),
      parents = Just (greatGrandMotherM, greatGrandMotherM)
    }

grandFatherM :: Person
grandFatherM =
  Person
    { firstName = "Innokentii",
      lastName = "Sidorov",
      formerLastNames = [],
      age = 85,
      document = Just (Passport (3333, 666777)),
      parents = Nothing
    }

mother :: Person
mother =
  Person
    { firstName = "Anna",
      lastName = "Ivanova",
      formerLastNames = [],
      age = 51,
      document = Just (Passport (2222, 444333)),
      parents = Just (grandMotherM, grandFatherM)
    }

greatGrandMotherF :: Person
greatGrandMotherF =
  Person
    { firstName = "Eleanora",
      lastName = "Ivanova",
      formerLastNames = [],
      age = 99,
      document = Just (Passport (2233, 444555)),
      parents = Nothing
    }

greatGrandFatherF :: Person
greatGrandFatherF =
  Person
    { firstName = "Zahar",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 105,
      document = Just (Passport (1233, 144555)),
      parents = Nothing
    }

grandMotherF :: Person
grandMotherF =
  Person
    { firstName = "Anastasija",
      lastName = "Ivanova",
      formerLastNames = [],
      age = 78,
      document = Just (Passport (2222, 999333)),
      parents = Just (greatGrandMotherF, greatGrandFatherF)
    }

grandFatherF :: Person
grandFatherF =
  Person
    { firstName = "Petr",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 88,
      document = Just (Passport (2222, 444888)),
      parents = Nothing
    }

father :: Person
father =
  Person
    { firstName = "Ivan",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 59,
      document = Just (Passport (3333, 444333)),
      parents = Just (grandMotherF, grandFatherF)
    }

fatherAnother :: Person
fatherAnother =
  Person
    { firstName = "Gerasim",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 55,
      document = Just (Passport (3398, 445337)),
      parents = Just (grandFatherF, grandFatherF)
    }

personWithoutParents :: Person
personWithoutParents =
  Person
    { firstName = "Mark",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 21,
      document = Just (Passport (1111, 444333)),
      parents = Nothing
    }

personWithParents :: Person
personWithParents =
  Person
    { firstName = "Mark",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 21,
      document = Just (Passport (1111, 444333)),
      parents = Just (mother, father)
    }

personWithFather :: Person
personWithFather =
  Person
    { firstName = "Mark Again",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 19,
      document = Just (Passport (9111, 944333)),
      parents = Just (fatherAnother, fatherAnother)
    }

child1 :: Person
child1 =
  Person
    { firstName = "Mark II",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 1,
      document = Just (BirthCertificate 1111),
      parents = Just (personWithoutParents, personWithoutParents)
    }

child2 :: Person
child2 =
  Person
    { firstName = "Mark III",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 2,
      document = Just (BirthCertificate 2222),
      parents = Just (personWithoutParents, personWithoutParents)
    }
  
child3 :: Person
child3 =
  Person
    { firstName = "Mark Again I",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 2,
      document = Just (BirthCertificate 3333),
      parents = Just (personWithFather, personWithFather)
    }

unit_createChild :: Assertion
unit_createChild = do
  personWithParents @?= createChild (mother, father) personWithoutParents

unit_ancestors :: IO ()
unit_ancestors = do
  Set.empty @?= ancestors 0 personWithoutParents
  Set.empty @?= ancestors 3 personWithoutParents
  Set.fromList [mother, father] @?= ancestors 0 personWithParents
  Set.fromList [grandMotherM, grandFatherM, grandMotherF, grandFatherF] @?= ancestors 1 personWithParents
  Set.fromList [greatGrandMotherM, greatGrandMotherF, greatGrandFatherF] @?= ancestors 2 personWithParents
  Set.empty @?= ancestors 3 personWithParents

unit_greatestAncestor :: IO ()
unit_greatestAncestor = do
  personWithoutParents @?= greatestAncestor personWithoutParents
  greatGrandFatherF @?= greatestAncestor personWithParents

setWithChild1 :: Set Person
setWithChild1 = Set.fromList [child1]

setWithChild1Tree :: Set (Tree Person)
setWithChild1Tree = Set.singleton (Tree child1 Set.empty)

setWithChild1And2Tree :: Set (Tree Person)
setWithChild1And2Tree = Set.fromList [Tree child1 Set.empty, Tree child2 Set.empty]

setWithChild1And2 :: Set Person
setWithChild1And2 = Set.fromList [child1, child2]

personWithParentsTree :: Tree Person
personWithParentsTree = Tree personWithParents Set.empty

setWithPersonWithParentsTree :: Set (Tree Person)
setWithPersonWithParentsTree = Set.singleton personWithParentsTree

personWithParentsAndMotherTree :: Tree Person
personWithParentsAndMotherTree = Tree mother setWithPersonWithParentsTree

setPersonWithParentsAndMotherTree :: Set (Tree Person)
setPersonWithParentsAndMotherTree = Set.singleton personWithParentsAndMotherTree

setWithAllRelativesForPersonWithParents :: Set Person
setWithAllRelativesForPersonWithParents =
  Set.fromList
    [ greatGrandMotherM,
      grandMotherM,
      grandFatherM,
      mother,
      personWithParents,
      father,
      grandMotherF,
      grandFatherF,
      greatGrandFatherF
    ]

-- grandFatherF  ->  father        ->  personWithParents
--               \-> fatherAnother \-> personWithFather   -> child3


fatherTree :: Tree Person
fatherTree = Tree father (Set.singleton personWithParentsTree)

personWithFatherTree :: Tree Person
personWithFatherTree = Tree personWithFather (Set.singleton (Tree child3 Set.empty))

fatherAnotherTree :: Tree Person
fatherAnotherTree = Tree fatherAnother (Set.singleton personWithFatherTree)

grandFatherFDescendantsTree :: Tree Person
grandFatherFDescendantsTree = Tree grandFatherF (Set.fromList [fatherTree, fatherAnotherTree])

setWithAllPersons :: Set Person
setWithAllPersons =
  Set.union
    setWithAllRelativesForPersonWithParents
    ( Set.fromList
        [ personWithoutParents,
          personWithFather,
          fatherAnother,
          child1,
          child2,
          child3
        ]
    )

unit_descendants :: Assertion
unit_descendants = do
  Tree personWithoutParents Set.empty @?= descendants personWithoutParents Set.empty
  Tree personWithoutParents setWithChild1Tree @?= descendants personWithoutParents setWithChild1
  Tree personWithoutParents setWithChild1And2Tree @?= descendants personWithoutParents setWithChild1And2
  Tree mother setWithPersonWithParentsTree @?= descendants mother setWithAllRelativesForPersonWithParents
  grandFatherFDescendantsTree @?= descendants grandFatherF setWithAllPersons
