module Test.Person where

import qualified Data.Set as Set
import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person

paulSkinner =
  Person { firstName = "Paul"
         , lastName = "Skinner"
         , formerLastNames = []
         , age = 52
         , idNumber = Just (Passport 1234 567890)
         , parents = (Nothing, Nothing) }

emmaSkinner =
  Person { firstName = "Emma"
         , lastName = "Skinner"
         , formerLastNames = ["Jarvis"]
         , age = 50
         , idNumber = Just (MiscDocument "Загранпаспорт: 12 345678")
         , parents = (Nothing, Nothing) }

georgeSkinner =
  Person { firstName = "George"
         , lastName = "Skinner"
         , formerLastNames = []
         , age = 25
         , idNumber = Nothing
         , parents = (Just emmaSkinner, Just paulSkinner) }

linaBrown =
  Person { firstName = "Lina"
         , lastName = "Brown"
         , formerLastNames = []
         , age = 24
         , idNumber = Just (Passport 4312 231341)
         , parents = (Nothing, Nothing) }

ivanSkinner =
  Person { firstName = "Ivan"
         , lastName = "Skinner"
         , formerLastNames = []
         , age = 0
         , idNumber = Nothing
         , parents = (Just linaBrown, Just georgeSkinner) }

everyone = Set.fromList [paulSkinner, emmaSkinner, georgeSkinner, linaBrown, ivanSkinner]

unit_createChild = do 
  let ivanSkinner' = createChild ((linaBrown, georgeSkinner))
  ivanSkinner @?= ivanSkinner'

unit_greatestAncestor = do
  greatestAncestor ivanSkinner @?= paulSkinner
  greatestAncestor georgeSkinner @?= paulSkinner
  greatestAncestor linaBrown @?= linaBrown
  greatestAncestor paulSkinner @?= paulSkinner
  greatestAncestor emmaSkinner @?= emmaSkinner

unit_ancestors = do
  ancestors 0 ivanSkinner @?= Set.fromList [ivanSkinner] 
  ancestors 1 ivanSkinner @?= Set.fromList [linaBrown, georgeSkinner] 
  ancestors 2 ivanSkinner @?= Set.fromList [paulSkinner, emmaSkinner] 
  ancestors 3 ivanSkinner @?= Set.empty 

unit_descendants = do
  let paulTree = descendants paulSkinner everyone
  me paulTree @?= paulSkinner
  Set.size (children paulTree) @?= 1

  let georgeTree = head (Set.elems (children paulTree))
  me georgeTree @?= georgeSkinner
  Set.size (children georgeTree) @?= 1

  let ivanTree = head (Set.elems (children georgeTree))
  me ivanTree @?= ivanSkinner
  Set.size (children ivanTree) @?= 0
