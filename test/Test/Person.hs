module Test.Person where

import Test.Tasty.HUnit ((@?=))
import Person
import qualified Data.Set as Set

passport1 :: Document
passport1 = Passport 1234 56789

passport2 :: Document
passport2 = Passport 4312 52345

birthCertificate1 :: Document
birthCertificate1 = BirthCertificate 1234 56789

{-
    Phil Collins  💍 Peter Gabriel
               /      \
              /        \
   Michael Jackson      Elton John 💍  David Furnish
                                    |
                               Justin Bieber
-}

philCollins :: Person
philCollins =
  Person { firstName = "Phil"
         , lastName = "Collins"
         , formerLastNames = []
         , age = 71
         , idNumber = Just passport1
         , parents = Set.empty }

peterGabriel :: Person
peterGabriel =
  Person { firstName = "Peter"
         , lastName = "Gabriel"
         , formerLastNames = []
         , age = 72
         , idNumber = Just passport1
         , parents = Set.empty }

michaelJackson :: Person
michaelJackson =
  Person { firstName = "Michael"
         , lastName = "Jackson"
         , formerLastNames = []
         , age = 0
         , idNumber = Just birthCertificate1
         , parents = Set.fromList [philCollins, peterGabriel] }

eltonJohn :: Person
eltonJohn =
  Person { firstName = "Elton"
         , lastName = "John"
         , formerLastNames = []
         , age = 0
         , idNumber = Just birthCertificate1
         , parents = Set.fromList [philCollins, peterGabriel] }

davidFurnish :: Person
davidFurnish =
  Person { firstName = "David"
         , lastName = "Furnish"
         , formerLastNames = []
         , age = 59
         , idNumber = Nothing
         , parents = Set.empty }

justinBieber :: Person
justinBieber =
  Person { firstName = "Justin"
         , lastName = "Bieber"
         , formerLastNames = []
         , age = 28
         , idNumber = Just passport1
         , parents = Set.fromList [eltonJohn, davidFurnish] }

allPeople :: Set.Set Person
allPeople = Set.fromList [philCollins, peterGabriel, michaelJackson, eltonJohn, davidFurnish, justinBieber]

justinBieberTree :: Tree Person
justinBieberTree = Tree justinBieber Set.empty

eltonJohnTree :: Tree Person
eltonJohnTree = Tree eltonJohn (Set.fromList [justinBieberTree])

michaelJacksonTree :: Tree Person
michaelJacksonTree = Tree michaelJackson Set.empty

philCollinsTree :: Tree Person
philCollinsTree = Tree philCollins (Set.fromList [eltonJohnTree, michaelJacksonTree])

unit_createChild = do
  let michaelJackson' = createChild (Set.fromList [philCollins, peterGabriel]) "Michael" "Jackson" (Just birthCertificate1)
  let eltonJohn' = createChild (Set.fromList [peterGabriel, philCollins]) "Elton" "John" (Just birthCertificate1)
  michaelJackson @?= michaelJackson'
  eltonJohn @?= eltonJohn'

unit_greatestAncestor = do
  peterGabriel @?= greatestAncestor justinBieber  -- Peter Gabriel is older than Phil Collins
  peterGabriel @?= greatestAncestor eltonJohn
  philCollins @?= greatestAncestor philCollins
  davidFurnish @?= greatestAncestor davidFurnish
  
unit_ancestors = do
  Set.fromList [justinBieber] @?= ancestors 0 justinBieber
  Set.fromList [eltonJohn, davidFurnish] @?= ancestors 1 justinBieber
  Set.fromList [philCollins, peterGabriel] @?= ancestors 2 justinBieber
  Set.empty @?= ancestors 3 justinBieber
  Set.empty @?= ancestors 4 justinBieber
  Set.empty @?= ancestors (-1) justinBieber

unit_descendants = do
  justinBieberTree @?= descendants justinBieber allPeople
  eltonJohnTree @?= descendants eltonJohn allPeople
  michaelJacksonTree @?= descendants michaelJackson allPeople
  philCollinsTree @?= descendants philCollins allPeople
