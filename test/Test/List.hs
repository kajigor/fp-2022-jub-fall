
module Test.List where

import Test.Tasty.HUnit (Assertion, (@?=))

import List

fromList :: [a] -> List a
fromList = foldr AtLeastOne Empty

toList :: List a -> [a]
toList Empty = []
toList (AtLeastOne x xs) = x : toList xs

unit_sumAndMult :: Assertion
unit_sumAndMult = do
    testSumAndMult [] (0, 1)
    testSumAndMult [13] (13, 13)
    testSumAndMult [1,2,3,4,5] (15, 120)
    testSumAndMult [1,2,0,3,4,5] (15, 0)
    testSumAndMult [-2,-1,0,1,2] (0, 0)
  where
    testSumAndMult xs expected = sumAndMult (fromList xs) @?= expected

unit_maxNum :: Assertion
unit_maxNum = do
    testMaxNum [] 0
    testMaxNum [13] 13
    testMaxNum [1,2,3,4,5] 5
    testMaxNum [5,4,3,2,1] 5
    testMaxNum [1, -2, 3, -4, 5] 5
    testMaxNum [-1, 2, -3, 4, -5] 4
  where
    testMaxNum xs expected = maxNum (fromList xs) @?= expected

unit_append :: Assertion
unit_append = do
    testAppend [] [4,5] [4,5]
    testAppend [1,2,3] [] [1,2,3]
    testAppend [1,2,3] [4,5] [1,2,3,4,5]
  where
    testAppend xs ys expected = toList (append (fromList xs) (fromList ys)) @?= expected

