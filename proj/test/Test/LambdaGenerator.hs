module Test.LambdaGenerator where

import Lambda

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

genLetter :: Gen String
genLetter = Gen.element (map (\x -> [x]) ['a'..'z'])

genLambda :: Gen (Lambda String)
genLambda =
  Gen.recursive
    Gen.choice
    [
        genVar
    ]
    [
        genAbs,
        genApp
    ]
  where
    genVar = Lambda.Var <$> genLetter
    genAbs = do
      lt <- (Lambda.Abs <$> genLetter)
      Gen.subterm genLambda lt
    genApp = do
      Gen.subterm2 genLambda genLambda (Lambda.App)
