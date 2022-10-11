module Test.Person where

import qualified Data.Set         as Set
import           Person           (Document (..), Person (..),
                                   SurnameStrategy (..), Tree (..), ancestors,
                                   createChild, descendants, greatestAncestor)
import           Test.Tasty.HUnit (assertBool)

pGosling :: Person
pGosling =
  Person
    { firstName = "Ryan"
    , lastName = "Gosling"
    , formerLastNames = []
    , age = 41
    , idNumber = Nothing
    , parents = (Nothing, Nothing)
    }

pEmma :: Person
pEmma =
  Person
    { firstName = "Emma"
    , lastName = "Stone"
    , formerLastNames = []
    , age = 33
    , idNumber = Nothing
    , parents = (Nothing, Nothing)
    }

pBurunov :: Person
pBurunov =
  Person
    { firstName = "Sergei"
    , lastName = "Burunov"
    , formerLastNames = []
    , age = 45
    , idNumber = Nothing
    , parents = (Nothing, Nothing)
    }

pDr :: Person
pDr =
  Person
    { firstName = "Doctor"
    , lastName = "Katsuragi"
    , formerLastNames = []
    , age = 48
    , idNumber = Nothing
    , parents = (Nothing, Nothing)
    }

pMisato :: Person
pMisato =
  createChild
    (Just pDr)
    Nothing
    "Misato"
    Nothing
    (Just (Minor ("XV", "АБ", 123456)))

pMiron :: Person
pMiron =
  createChild
    (Just pMisato)
    (Just pGosling)
    "Miron"
    (Just Combined)
    (Just (Minor ("IV", "ГБ", 123456)))

pAnna :: Person
pAnna =
  createChild
    (Just pEmma)
    (Just pBurunov)
    "Anna"
    (Just Parent2)
    (Just (Minor ("IV", "ГБ", 654321)))

pIvan :: Person
pIvan =
  createChild
    (Just pMiron)
    (Just pAnna)
    "Ivan"
    (Just Parent1)
    (Just (Minor ("IV", "ГБ", 000999)))

pOleg :: Person
pOleg =
  createChild
    (Just pMiron)
    (Just pAnna)
    "Oleg"
    (Just Parent1)
    (Just (Minor ("IV", "ГБ", 999000)))

{-
    Dr.Katsuragi
        |
        |
Misato Katsuragi-----Rayan Gosling            Emma Stone-------Sergei Burunov
                  |                                        |
                  |                                        |
                Miron Katsuragi-Gosling-----------------Anna Burunov
                                                |
                                                |
                                               / \
                                              /   \
                                             /     \
                                           Ivan   Oleg
                                        Katsuragi-Gosling
-}
unit_lastnames = do
  assertBool "single parent" (lastName pMisato == "Katsuragi")
  assertBool "double surname" (lastName pMiron == "Katsuragi-Gosling")
  assertBool "second parent surname" (lastName pAnna == "Burunov")
  assertBool
    "double surname transfered from p1 to children"
    (lastName pMiron == "Katsuragi-Gosling")
  assertBool "siblings have same surname" (lastName pIvan == lastName pOleg)

unit_greatest_ancestor = do
  assertBool "self" (greatestAncestor pDr == (pDr, 0))
  assertBool "age based" (greatestAncestor pAnna == (pBurunov, 1))
  assertBool "level based" (greatestAncestor pIvan == (pDr, 3))
  assertBool "siblings" (greatestAncestor pIvan == greatestAncestor pOleg)

unit_ancestors = do
  assertBool "self" (ancestors 0 pDr == Set.fromList [pDr])
  assertBool "too high" (ancestors 2 pMisato == Set.empty)
  assertBool
    "normal test"
    (ancestors 2 pIvan == Set.fromList [pMisato, pGosling, pEmma, pBurunov])

unit_tree = do
  let allPeopleSet =
        Set.fromList
          [pGosling, pEmma, pBurunov, pDr, pMisato, pMiron, pAnna, pIvan, pOleg]
  assertBool "self" (descendants allPeopleSet pIvan == Leaf pIvan)
  assertBool
    "1 level"
    (descendants allPeopleSet pMiron ==
     Node pMiron (Set.fromList [Leaf pIvan, Leaf pOleg]))
  let (Node _ tree1) = descendants allPeopleSet pMiron
  let (Node _ tree2) = descendants allPeopleSet pAnna
  assertBool "parent results are equal" (tree1 == tree2)
  assertBool
    "big test"
    (descendants allPeopleSet pDr ==
     Node
       pDr
       (Set.fromList
          [ Node
              pMisato
              (Set.fromList
                 [Node pMiron (Set.fromList [Leaf pIvan, Leaf pOleg])])
          ]))
--   assertBool String Bool
