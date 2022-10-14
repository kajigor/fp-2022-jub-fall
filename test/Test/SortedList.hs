module Test.SortedList where

import SortedList

import Test.HUnit (Assertion, assertBool, (@?=))

testMappend :: [Int] -> [Int] -> [Int] -> Assertion
testMappend xs ys exp =
  SortedList xs <> SortedList ys @?= SortedList exp

unit_monoid :: Assertion
unit_monoid = do
  testMappend []        []        []
  testMappend [1]       []        [1]
  testMappend [1]       [2]       [1, 2]
  testMappend [1,2]     [1]       [1, 1, 2]
  testMappend [1,3,5,7] [2,4,6,8] [1,2,3,4,5,6,7,8]

unit_mconcat :: Assertion
unit_mconcat = do
  mconcat (map SortedList [[1,2,3], [], [8,9], [5,6], [4], [7]]) @?= SortedList [1,2,3,4,5,6,7,8,9]
