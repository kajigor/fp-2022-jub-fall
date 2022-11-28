module Test.ParsePB where

import Lambda
import Parse
import Text.Megaparsec
import Data.List (sort)
import Hedgehog
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


prop_parseRandom :: Property
prop_parseRandom = property $ do
  term <- forAll $ genLambda
  let repr = show term
  let back = parseMaybe parseLambda repr
  assert (back == Just term)


props :: [TestTree]
props =
  [ testProperty "parse ∘ show = id" prop_parseRandom
  ]