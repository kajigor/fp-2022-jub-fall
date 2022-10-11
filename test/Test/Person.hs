module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set

-- grandparent 1
jenny :: Person
jenny =
  Person { firstName = "Jenny"
         , lastName = "James"
         , formerLastNames = []
         , age = 71
         , idDocument = Passport_ (Passport { passportSeries = 6298, passportNumber = 052017 })
         , parents = (Nothing, Nothing) }

-- grandparent 2 (greatestAncestor)
david :: Person
david =
  Person { firstName = "David"
         , lastName = "Bale"
         , formerLastNames = []
         , age = 81
         , idDocument = Passport_ (Passport { passportSeries = 2091, passportNumber = 492081 })
         , parents = (Nothing, Nothing) }

-- grandparent 3
slobodan :: Person
slobodan =
  Person { firstName = "Slobodan"
         , lastName = "Blažić"
         , formerLastNames = []
         , age = 75
         , idDocument = Passport_ (Passport { passportSeries = 1035, passportNumber = 205643 })
         , parents = (Nothing, Nothing) }

-- grandparent 4
nadezda :: Person
nadezda =
  Person { firstName = "Nadezda"
         , lastName = "Topalski"
         , formerLastNames = []
         , age = 74
         , idDocument = Passport_ (Passport { passportSeries = 9346, passportNumber = 179345 })
         , parents = (Nothing, Nothing) }

-- parent 1
sibi :: Person
sibi =
  Person { firstName = "Sibi"
         , lastName = "Blažić"
         , formerLastNames = []
         , age = 52
         , idDocument = Passport_ (Passport { passportSeries = 1234, passportNumber = 567890 })
         , parents = (Just slobodan, Just nadezda) }

-- parent 2
christian :: Person
christian =
  Person { firstName = "Christian"
         , lastName = "Bale"
         , formerLastNames = []
         , age = 48
         , idDocument = Passport_ (Passport { passportSeries = 3567, passportNumber = 891011 })
         , parents = (Just david, Just jenny) }


-- child 1
emmeline :: Person
emmeline =
  Person { firstName = "Emmeline"
         , lastName = "Bale"
         , formerLastNames = []
         , age = 17
         , idDocument = Passport_ (Passport { passportSeries = 5678, passportNumber = 910112 })
         , parents = (Just sibi, Just christian) }

-- child 2
joseph :: Person
joseph =
  Person { firstName = "Joseph"
         , lastName = "Bale"
         , formerLastNames = []
         , age = 8
         , idDocument = BirthCertificate_ (BirthCertificate { birthCertSeries = "V5", birthCertNumber = 301802 })
         , parents = (Just sibi, Just christian) }

-- child 3
michael :: Person
michael =
  Person { firstName = "Michael"
         , lastName = "Bale"
         , formerLastNames = []
         , age = 0
         , idDocument = BirthCertificate_ BirthCertificate { birthCertSeries = "M7", birthCertNumber = 201476 }
         , parents = (Just sibi, Just christian)}

unit_createChild = do
  createChild (Just sibi) (Just christian) "Michael" "Bale" (BirthCertificate_ BirthCertificate { birthCertSeries = "M7", birthCertNumber = 201476 }) @?= michael

unit_greatestAncestor = do
  greatestAncestor joseph @?= david
  greatestAncestor sibi @?= slobodan
  greatestAncestor emmeline @?= david
  greatestAncestor christian @?= david
  greatestAncestor david @?= david

unit_ancestors = do
  ancestors 1 jenny @?= Set.empty
  ancestors 1 sibi @?= Set.fromList [slobodan, nadezda]
  ancestors 2 sibi @?= Set.empty
  ancestors 1 christian @?= Set.fromList [david, jenny]
  ancestors 2 christian @?= Set.empty
  ancestors 1 emmeline @?= Set.fromList [sibi, christian]
  ancestors 2 emmeline @?= Set.fromList [slobodan, nadezda, david, jenny]
  ancestors 3 emmeline @?= Set.empty
  ancestors 1 joseph @?= Set.fromList [sibi, christian]
  ancestors 2 joseph @?= Set.fromList [slobodan, nadezda, david, jenny]
  ancestors 3 joseph @?= Set.empty

allPeople = Set.fromList [jenny, david, slobodan, nadezda, sibi, christian, emmeline, joseph, michael]

unit_descendants = do
  thisPerson (descendants david allPeople) @?= david
  Set.size (allDescendants (descendants david allPeople)) @?= 1
  Set.size (allDescendants (descendants slobodan allPeople)) @?= 1
  thisPerson (descendants emmeline allPeople) @?= emmeline
  Set.size (allDescendants (descendants emmeline allPeople)) @?= 0
  Set.size (allDescendants (descendants sibi allPeople)) @?= 3
  Set.size (allDescendants (descendants christian allPeople)) @?= 3
