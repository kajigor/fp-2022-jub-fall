module Test.Person where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person
import Data.Set

createPassport :: Int -> Int -> Document
createPassport s n = Passport { series = s, number = n }

createBirtCert :: Int -> Document
createBirtCert sn = BirthCertificate { series_number = sn }

createPerson :: String -> String -> Int -> Maybe Document -> (Maybe Person, Maybe Person) -> Person
createPerson name surname age doc parents = Person {
                                                firstName = name,
                                                lastName = surname,
                                                formerLastNames = [],
                                                age = age,
                                                idNumber = doc,
                                                parents = parents
                                            }

kirill :: Person
kirill = createPerson "Kirill" "Ivanov" 19 (Just (createPassport 1 1)) (Nothing, Nothing)
masha :: Person
masha = createPerson "Masha" "Ivanchura" 20 (Just (createPassport 1 2)) (Nothing, Nothing)
mark :: Person
mark = createPerson "Mark" "Prudnikov" 54 (Just (createPassport 1 3)) (Nothing, Nothing)
petr :: Person
petr = createPerson "Peter" "Tsvetkov" 100500 (Just (createPassport 1 3)) (Nothing, Nothing)
sasha :: Person
sasha = createPerson "Sanya" "Tukmachev" 12 (Just (createPassport 1 1111)) (Just kirill, Just masha)
ivan :: Person
ivan = createPerson "Ivan" "Orlov" 121 (Just (createPassport 1212 21)) (Just mark, Just petr)
t1 :: Person
t1 = createPerson "Test" "One" 13 (Just (createPassport 122 21)) (Nothing, Just ivan)
t2 :: Person
t2 = createPerson "Test" "two" 14 (Just (createPassport 122 22)) (Nothing, Just ivan)
t3 :: Person
t3 = createPerson "Tset" "Aboba" 5 (Just (createPassport 133322 22)) (Just t1, Just t2)
kid :: Person
kid = createPerson "Ilya" "Kid" 0 Nothing (Just kirill, Just masha)

unit_createChild :: Assertion
unit_createChild = do
    assertBool "Child!" (createChild "Ilya" "Kid" (Just kirill) (Just masha) == kid)

unit_greatestAncestor :: IO ()
unit_greatestAncestor = do
    greatestAncestor kirill @?= kirill
    greatestAncestor ivan @?= petr
    greatestAncestor sasha @?= masha
    greatestAncestor t1 @?= petr
    greatestAncestor t3 @?= petr
    greatestAncestor kid @?= masha

unit_ancestors :: IO ()
unit_ancestors = do
    ancestors 0 kirill @?= fromList [kirill]
    ancestors 1 sasha @?= fromList [kirill, masha]
    ancestors 2 t1 @?= fromList [petr, mark]
    ancestors 3 t3 @?= fromList [petr, mark]
    ancestors 1000 kirill @?= fromList []

persons :: Set Person
persons = fromList [kirill, masha, sasha, petr, ivan, mark, t1, t2, t3, kid]

unit_descendants :: IO ()
unit_descendants = do
    descendants persons t3 @?= Tree { node = t3, children = [] }
    descendants persons kirill @?= Tree {
        node = kirill,
        children = [Tree {node = kid, children = []}, Tree {node = sasha, children = []}]
    }    
    descendants persons petr @?= Tree {
        node = petr,
        children = [Tree {node = ivan, children = [
            Tree { node = t1, children=[Tree {node = t3, children = []}] },
            Tree { node = t2, children=[Tree {node = t3, children = []}] }
        ]}]
    }
