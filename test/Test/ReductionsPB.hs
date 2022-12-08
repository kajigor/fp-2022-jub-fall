module Test.ReductionsPB where

import Lambda
import Reductions
import Hedgehog
import Data.Maybe
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

genName :: Gen String
genName = Gen.list (Range.singleton 1) Gen.alphaNum

genLambda :: Gen Lambda
genLambda =
  Gen.recursive
    Gen.choice
    [ Lambda.Var <$> genName
    ]
    [ Gen.subtermM genLambda (\x -> Abs <$> genName <*> pure x),
      Gen.subterm2 genLambda genLambda App
    ]


prop_strategiesAreEquivalent :: Property
prop_strategiesAreEquivalent = property $ do
  term <- forAll $ genLambda
  let n = Just 1000
  let a = maybe Nothing (Just . toDeBruijn) (Reductions.evalMaybe NormalOrder term n)
  let b = maybe Nothing (Just . toDeBruijn)  (Reductions.evalMaybe ApplicativeOrder term n)
  assert (a == b)


props :: [TestTree]
props =
  [ testProperty "normal and applicative orders agree" prop_strategiesAreEquivalent
  ]