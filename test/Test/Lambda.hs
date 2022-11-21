{-# LANGUAGE InstanceSigs #-}
module Test.Lambda where

import Lambda (true, false, and, zero, two, Lambda(..), DeBruijn(..), toDeBruijn, fromDeBruijn, alphaEq, cas, Freshable(..), Subst(..), Strategy (..), eval)
import Test.HUnit
import Data.Hashable (Hashable)

data MyString = MyString String



instance Show MyString where
    show (MyString x) = x

justVar :: Lambda String
justVar = Var "x"

justVar2 :: Lambda MyString
justVar2 = Var $ MyString "x"


lambdaFailedShow :: Lambda String
lambdaFailedShow = App t t
    where
        t = Abs "x" (App (Var "x") (Var "x"))

true2 = Abs (MyString "x") (Abs (MyString "y") (Var (MyString "x")))

unit_show_string = do
    show justVar @?= "x"
    show true @?= "λx.λy.x"
    show Lambda.and @?= "λp.λq.p q p"
    show Lambda.zero @?= "λf.λx.x"
    show Lambda.two @?= "λf.λx.f (f x)"
    show lambdaFailedShow @?= "(λx.x x) (λx.x x)"

unit_show_poly = do
    show justVar2 @?= "x"
    show true2 @?= "λx.λy.x"

deBruijn1 :: DeBruijn
deBruijn1 = VarDB 0

deBruijn2 :: DeBruijn
deBruijn2 = AbsDB $ VarDB 0

deBruijn3 :: DeBruijn
deBruijn3 = AppDB (AbsDB $ VarDB 0) (VarDB 0)

unit_show_DeBruijn = do
    show deBruijn1 @?= "0"
    show deBruijn2 @?= "λ.0"
    show deBruijn3 @?= "(λ.0) 0"

trueDB :: DeBruijn
trueDB = AbsDB (AbsDB (VarDB 1))

trueDBtransformed :: Lambda Int
trueDBtransformed = Abs 0 (Abs 1 (Var 0))

andDB :: DeBruijn
andDB = AbsDB (AbsDB (AppDB (AppDB (VarDB 1) (VarDB 0)) (VarDB 1)))

andDBtransformed :: Lambda Int
andDBtransformed = Abs 0 (Abs 1 (App (App (Var 0) (Var 1)) (Var 0)))

zeroDB :: DeBruijn
zeroDB = AbsDB (AbsDB (VarDB 0))

zeroDBtransformed :: Lambda Int
zeroDBtransformed = Abs 0 (Abs 1 (Var 1))

twoDB :: DeBruijn
twoDB = AbsDB (AbsDB (AppDB (VarDB 1) (AppDB (VarDB 1) (VarDB 0))))

sample1 :: Lambda String
sample1 = App (Abs "x" (Abs "x" (Var "x"))) (Abs "x" (Var "x"))

sample1DB :: DeBruijn
sample1DB = AppDB (AbsDB $ AbsDB $ VarDB 0) (AbsDB $ VarDB 0)

sample1DBtransformed :: Lambda Int
sample1DBtransformed = App (Abs 0 (Abs 1 (Var 1))) (Abs 0 (Var 0))

failedToDeBruijn1 :: Lambda String
failedToDeBruijn1 = Abs "x" $ Var "x"

failedToDeBruijn2 :: Lambda String
failedToDeBruijn2 = Abs "x" $ Var "y"

unit_toDeBruijn = do
    toDeBruijn Lambda.true @?= trueDB
    toDeBruijn Lambda.and @?= andDB
    toDeBruijn Lambda.zero @?= zeroDB
    toDeBruijn Lambda.two @?= twoDB
    toDeBruijn sample1 @?= sample1DB
    toDeBruijn failedToDeBruijn1 @?= AbsDB (VarDB 0)
    toDeBruijn failedToDeBruijn2 @?= AbsDB (VarDB 1)

instance (Eq a, Hashable a) => Eq (Lambda a) where
    (==) :: (Eq a, Hashable a) => Lambda a -> Lambda a -> Bool
    (App a b) == (App c d) = a == c && b == d
    (Abs a b) == (Abs c d) = a == c && b == d
    (Var a) == (Var b) = a == b
    _ == _ = False

unit_alphaEq = do
    assertBool "Alpha Eq failed" (alphaEq (Var "x") (Var "y"))
    assertBool "Alpha Eq failed" (alphaEq (Abs "x" (Var "x")) (Abs "y" (Var "y")))
    assertBool "Alpha Eq failed" (alphaEq trueDBtransformed true)
    assertBool "Alpha Eq failed" (alphaEq andDBtransformed Lambda.and)
    assertBool "Alpha Eq failed" (alphaEq zeroDBtransformed zero)
    assertBool "Alpha Eq failed" (alphaEq sample1DBtransformed sample1)

    assertBool "Alpha Eq failed" (not $ alphaEq true false)
    assertBool "Alpha Eq failed" (not $ alphaEq zero two)

unit_fromDeBruijn = do
    fromDeBruijn trueDB @?= trueDBtransformed
    fromDeBruijn andDB @?= andDBtransformed
    fromDeBruijn zeroDB @?= zeroDBtransformed
    fromDeBruijn sample1DB @?= sample1DBtransformed

withFree :: Lambda String
withFree = Abs "x" $ Var "y"

unit_freshable_string = do
    getNext "y" ["x"] @?= "y"
    getNext "x" ["x"] @?= "x'"
    getNext "z" ["x", "x'"] @?= "z"
    getNext "x'" ["x", "y", "x'"] @?= "x''"

sampleFromLecture :: Lambda String
sampleFromLecture = Abs "x" $ App (Var "x") (Var "y")

sampleFromLectureReduced :: Lambda String
sampleFromLectureReduced = Abs "x'" $ App (Var "x'") (Var "x")

tooComplexExample :: Lambda String
tooComplexExample = App (Abs "x" $ App (Var "y") (Var "x")) (Abs "y" $ Var "y")

unit_cas = do
    cas withFree Subst {from = "z", to = Var "ahahahahhahahahahahahhahahahahahahahahahhaahahha"} @?= withFree
    cas sampleFromLecture Subst {from = "y", to = Var "x"} @?= sampleFromLectureReduced
    cas tooComplexExample Subst {from = "x", to = Var "z"} @?= tooComplexExample
    cas tooComplexExample Subst {from = "y", to = Var "z"} @?= App (Abs "x" $ App (Var "z") (Var "x")) (Abs "y" $ Var "y")

evalSample1 :: Lambda String
evalSample1 = App (Abs "x" $ Var "x") $ Var "y"

evalSample2 :: Lambda String
evalSample2 = App (App true $ Var "x'") $ Var "y'"

evalSample3 :: Lambda String
evalSample3 = Abs "x" (App (Abs "y" $ Var "x") $ Var "x")

evalSample4 :: Lambda String
evalSample4 = App (Var "x") (App true $ Var "z")
unit_eval = do
    eval CallByValue evalSample1 @?= Var "y"
    eval CallByValue evalSample2 @?= Var "x'"
    eval CallByValue evalSample3 @?= evalSample3
    eval CallByValue evalSample4 @?= App (Var "x") (Abs "y" $ Var "z")

    eval CallByName evalSample1 @?= Var "y"
    eval CallByName evalSample2 @?= Var "x'"
    eval CallByName evalSample3 @?= evalSample3
    eval CallByName evalSample4 @?= evalSample4

    eval NormalOrder evalSample1 @?= Var "y"
    eval NormalOrder evalSample2 @?= Var "x'"
    eval NormalOrder evalSample3 @?= Abs "x" (Var "x")
    eval NormalOrder evalSample4 @?= App (Var "x") (Abs "y" $ Var "z")

    eval ApplicativeOrder evalSample1 @?= Var "y"
    eval ApplicativeOrder evalSample2 @?= Var "x'"
    eval ApplicativeOrder evalSample3 @?= Abs "x" (Var "x")
    eval ApplicativeOrder evalSample4 @?= App (Var "x") (Abs "y" $ Var "z")