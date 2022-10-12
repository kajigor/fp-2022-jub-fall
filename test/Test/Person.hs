module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set

redtornado :: Person
redtornado =
  Person {
    firstName = "John",
    lastName = "Smith",
    formerLastNames = [],
    age = 1000,
    idNumber = Nothing,
    parents = Set.empty
  }

batman :: Person
batman =
  Person {
    firstName = "Bruce",
    lastName = "Wayne",
    formerLastNames = [],
    age = 19,
    idNumber = Nothing,
    parents = Set.empty
  }

catwoman :: Person
catwoman =
  Person {
    firstName = "Selina",
    lastName = "Kyle",
    formerLastNames = [],
    age = 20,
    idNumber = Nothing,
    parents = Set.empty
  }

superman :: Person
superman =
  Person {
    firstName = "Clark",
    lastName = "Kent",
    formerLastNames = [],
    age = 60,
    idNumber = Nothing,
    parents = Set.singleton redtornado
  }

wonderwoman :: Person
wonderwoman =
  Person {
    firstName = "Diana",
    lastName = "of Themyscira",
    formerLastNames = [],
    age = 60,
    idNumber = Nothing,
    parents = Set.empty
  }

flash :: Person
flash =
  Person {
    firstName = "Barry",
    lastName = "Allen",
    formerLastNames = [],
    age = 30,
    idNumber = Nothing,
    parents = Set.fromList [batman, catwoman]
  }

greenlantern :: Person
greenlantern =
  Person {
    firstName = "Hal",
    lastName = "Jordan",
    formerLastNames = [],
    age = 30,
    idNumber = Nothing,
    parents = Set.singleton superman
  }

atom :: Person
atom =
  Person {
    firstName = "Ray",
    lastName = "Palmer",
    formerLastNames = [],
    age = 30,
    idNumber = Nothing,
    parents = Set.singleton superman
  }

zatanna :: Person
zatanna =
  Person {
    firstName = "Zatanna",
    lastName = "Zatara",
    formerLastNames = [],
    age = 30,
    idNumber = Nothing,
    parents = Set.empty
  }

blackcanary :: Person
blackcanary =
  Person {
    firstName = "Dinah",
    lastName = "Laurel",
    formerLastNames = ["Lance"],
    age = 30,
    idNumber = Nothing,
    parents = Set.fromList [batman, catwoman]
  }

aquaman :: Person
aquaman =
  Person {
    firstName = "Arthur",
    lastName = "Curry",
    formerLastNames = [],
    age = 1,
    idNumber = Nothing,
    parents = Set.fromList [zatanna, flash]
  }

martianmanhunter :: Person
martianmanhunter =
  Person {
    firstName = "John",
    lastName = "Jones",
    formerLastNames = [],
    age = 0,
    idNumber = Just (BirthCertificate "A" 1),
    parents = Set.fromList [blackcanary, greenlantern]
  }

martianmanhunter2 = createChild (Set.fromList [blackcanary, greenlantern]) "John" "Jones" (BirthCertificate "A" 1)

invalidChild1 = createChild (Set.fromList [blackcanary, greenlantern]) "" "Jones" (BirthCertificate "A" 1)

invalidChild2 = createChild (Set.fromList [blackcanary, greenlantern]) "John" "" (BirthCertificate "A" 1)

everybody = Set.fromList [redtornado, batman, catwoman, superman, wonderwoman, flash, greenlantern, atom, zatanna, blackcanary, aquaman, martianmanhunter]

easyTree person list = Tree person (Set.fromList list)

treeBatman = easyTree batman [easyTree flash [easyTree aquaman []], easyTree blackcanary [easyTree martianmanhunter []]]

treeRedtornado = easyTree redtornado [easyTree superman [easyTree greenlantern [easyTree martianmanhunter []], easyTree atom []]]

treeZatanna = easyTree zatanna [easyTree aquaman []]


unit_createChild = do
  martianmanhunter2 @?= Just martianmanhunter
  invalidChild1 @?= Nothing
  invalidChild2 @?= Nothing

unit_greatestAncestor = do
  greatestAncestor flash @?= Just catwoman
  greatestAncestor aquaman @?= Just catwoman
  greatestAncestor atom @?= Just redtornado
  greatestAncestor martianmanhunter @?= Just redtornado
  greatestAncestor zatanna @?= Nothing

unit_ancestors = do
  ancestors (-1) martianmanhunter @?= Set.empty
  ancestors 0 martianmanhunter @?= Set.empty
  ancestors 1 martianmanhunter @?= Set.fromList [blackcanary, greenlantern]
  ancestors 2 martianmanhunter @?= Set.fromList [batman, catwoman, superman]
  ancestors 3 martianmanhunter @?= Set.singleton redtornado
  ancestors 4 martianmanhunter @?= Set.empty

unit_descendants = do
  descendants batman everybody @?= treeBatman
  descendants redtornado everybody @?= treeRedtornado
  descendants zatanna everybody @?= treeZatanna
