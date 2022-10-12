{-# LANGUAGE NumericUnderscores #-}
module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import qualified Data.Set as Set
import Data.Maybe 


createPerson :: String -> String -> Int -> Document -> Maybe Person -> Maybe Person -> Person
createPerson firstName lastName age doc father mother = Person {
    firstName = firstName,
    lastName = lastName,
    formerLastNames = [],
    age = age,
    document = doc,
    parents = Parents father mother
}

myGrandMotherM = createPerson "myMother'sMother" "" 1000 (Passport 0001 1234_5678) Nothing Nothing 
myGrandMotherF = createPerson "myFather'sMother" "" 90 (Passport 0002 1234_5678) Nothing Nothing 
myGrandFatherM = createPerson "myMother'sFather" "" 999_999 (Passport 0003 1234_5678) Nothing Nothing 
myGrandFatherF = createPerson "myFather'sFather" "" 91 (Passport 0004 1234_5678) Nothing Nothing 

myFather = createPerson "myFather" "myFather'sSurname" 10 (Passport 0005 1234_0000) (Just myGrandFatherF) (Just myGrandMotherF) 
myMother = createPerson "myMother" "myMother'sSurname" 20 (Passport 0006 1234_0000) (Just myGrandFatherM) (Just myGrandMotherM) 

me = createPerson "me" "mySurname" 200 (Passport 0000 0000_0000) (Just myFather) (Just myMother)
myPedigree = Node (Node (Leaf myGrandFatherF) myFather (Leaf myGrandMotherF)) me (Node (Leaf myGrandFatherM) myMother (Leaf myGrandMotherM))

orphan = createPerson "lone" "butHappy" 7 (Passport 6666 6666_6666) Nothing Nothing

hasOnlyFather = createPerson "name" "father'sSurname" 13 (Passport 6666 0001_0001) (Just myFather) Nothing
hasOnlyFatherPedigree = LeftNode (Node (Leaf myGrandFatherF) myFather (Leaf myGrandMotherF)) hasOnlyFather
hasOnlyMother = createPerson "myFather" "wentForCigarettes" 13 (Passport 6666 1001_1001) Nothing (Just myMother)

son = createPerson "son" "" 21 (Passport 6688 0001_0001) (Just father') Nothing
father' = createPerson "father" "" 21 (Passport 6688 1001_1001) Nothing Nothing
sonFatherPedigree = LeftNode (Leaf father') son

child :: Person
child = createPerson "SmallAndSweety" "myFather'sSurname" 0 (BirthCert 123) (Just myFather) (Just myMother)


unit_createChild = do
    createChild myFather myMother "SmallAndSweety" (BirthCert 123) @?= child

unit_greatestAncestor = do
    greatestAncestor hasOnlyFather @?= myGrandFatherF
    greatestAncestor hasOnlyMother @?= myGrandFatherM
    greatestAncestor me @?= myGrandFatherM

unit_ancestors = do
    ancestors 2 me @?= Set.fromList [myGrandFatherF, myGrandMotherF, myGrandFatherM, myGrandMotherM]
    ancestors 1 me @?= Set.fromList [myFather, myMother]
    ancestors 0 me @?= Set.fromList [me]



unit_descendants = do
    descendants me @?= myPedigree
    descendants hasOnlyFather @?= hasOnlyFatherPedigree
    descendants son @?= sonFatherPedigree
