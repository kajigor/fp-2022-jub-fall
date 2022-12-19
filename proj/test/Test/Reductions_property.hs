module Test.Reductions_property where

import Reductions
import Lambda
import Data.Maybe

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Test.Parser_property

reduce_until :: Strategy -> Lambda.Lambda String -> Lambda.Lambda String
reduce_until strategy term = (term_reduced term1)
    where term1 = reductor strategy term
          term_reduced (Just term_new) = (reduce_until strategy term_new)
          term_reduced Nothing = term  

isNormalCorrect :: Lambda.Lambda String -> Bool
isNormalCorrect (Lambda.App (Lambda.Abs x a) b) = False
isNormalCorrect (Lambda.App a b) = (isNormalCorrect a) && (isNormalCorrect b)
isNormalCorrect (Lambda.Var a) = True
isNormalCorrect (Lambda.Abs x a) = (isNormalCorrect a)

isWHNFCorrect :: Lambda.Lambda String -> Bool
isWHNFCorrect (App a b) = isWHNFCorrect a
isWHNFCorrect _ = True

isWNFCorrect :: Lambda.Lambda String -> Bool
isWNFCorrect (App a b) = (isWNFCorrect a) && (isWNFCorrect b)
isWNFCorrect _ = True


totalCorrectReduce :: MonadTest m => (Lambda.Lambda String -> Bool) -> (Lambda.Lambda String -> (Lambda.Lambda String)) -> Lambda.Lambda String -> m ()
totalCorrectReduce isCorrect reducer lambda = ((isCorrect (reducer lambda)) === True)

prop_reduce_normal :: Property
prop_reduce_normal = property $ do
  lambda <- forAll $ genLambda
  totalCorrectReduce isNormalCorrect (reduce_until NormalOrder) lambda

prop_reduce_applicative :: Property
prop_reduce_applicative = property $ do
  lambda <- forAll $ genLambda
  totalCorrectReduce isNormalCorrect (reduce_until ApplicativeOrder) lambda

prop_reduce_CallByName :: Property
prop_reduce_CallByName = property $ do
  lambda <- forAll $ genLambda
  totalCorrectReduce isWHNFCorrect (reduce_until CallByName) lambda

prop_reduce_CallByValue :: Property
prop_reduce_CallByValue = property $ do
  lambda <- forAll $ genLambda
  totalCorrectReduce isWNFCorrect (reduce_until CallByValue) lambda


props :: [TestTree]
props =
  [ testProperty "prop_reduce_normal terminates in NF" prop_reduce_normal,
   testProperty "prop_reduce_applicative terminates in NF" prop_reduce_applicative,
   testProperty "prop_reduce_CallByName terminates in WHNF" prop_reduce_CallByName,
   testProperty "prop_reduce_CallByValue terminates in WNF" prop_reduce_CallByValue
  ]