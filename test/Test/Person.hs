module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import Data.Set

createPerson :: String -> String -> Int -> Int -> (Maybe Person, Maybe Person) -> Person
createPerson name surname age id parents = Person {
    firstName = name,
    lastName = surname,
    formerLastNames = [],
    age = age,
    idNumber = Just (Document id),
    parents = parents
}

bb :: Person
bb = createPerson "Berend" "Bohn" 50 143587 (Nothing, Nothing)
vm :: Person
vm = createPerson "Viktoria" "Mayer" 45 234983 (Nothing, Nothing)
hl :: Person
hl = createPerson "Hraban" "Larenz" 47 234983 (Nothing, Nothing)
ho :: Person
ho = createPerson "Henrike" "Otto" 48 485838 (Nothing, Nothing)
md :: Person
md = createPerson "Menne" "Dickson" 25 867342 (Just bb, Just vm)
gb :: Person
gb = createPerson "Grete" "Bauer" 22 192837 (Just hl, Just ho)
al :: Person
al = createPerson "Ada" "Luther" 100 989795 (Nothing, Nothing)
lm :: Person
lm = createPerson "Lothur" "Muller" 31 384950 (Nothing, Just vm)
eg :: Person
eg = createPerson "Elke" "Gerven" 5 12345 (Just lm, Just gb)
js :: Person
js = createPerson "Jurgen" "Shmidt" 3 54321 (Just md, Just al)
kd :: Person
kd = createChild md gb "Klaus"

unit_greatestAncestor = do
    greatestAncestor vm @?= vm
    greatestAncestor md @?= bb
    greatestAncestor gb @?= ho
    greatestAncestor eg @?= ho
    greatestAncestor js @?= bb
    greatestAncestor kd @?= bb

unit_ancestors = do
    ancestors 0 kd @?= fromList [kd]
    ancestors 1 kd @?= fromList [md, gb]
    ancestors 2 kd @?= fromList [bb, vm, hl, ho]
    ancestors 4 kd @?= fromList []
    ancestors 1000 kd @?= fromList []
    ancestors (-1) kd @?= fromList []
    ancestors 2 js @?= fromList [bb, vm]
    ancestors 2 eg @?= fromList [vm, hl, ho]

unit_isParent = do
    isParent bb md @?= True
    isParent ho gb @?= True
    isParent hl md @?= False
    isParent md md @?= False
    isParent js md @?= False
    isParent eg md @?= False
    isParent bb kd @?= False
    isParent bb eg @?= False

people = fromList [bb, vm, hl, ho, lm, md, gb, al, eg, kd, js]

unit_descendants = do
    descendants kd people @?= Tree kd empty
    descendants md people @?= Tree md (fromList [Tree kd empty, Tree js empty])
    descendants ho people @?= Tree ho (fromList [Tree gb (fromList [Tree eg empty, Tree kd empty])])
