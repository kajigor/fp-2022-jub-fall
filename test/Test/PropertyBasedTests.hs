module Test.PropertyBasedTests where

import Minesweeper
import Data.Set as Set
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import Data.Set as Set

genInt :: Int -> Int -> Gen Int
genInt left right = Gen.int (Range.constant left right)

getRndList :: Gen [Int]
getRndList = Gen.list (Range.constant 500 500) (genInt 0 2147483647) -- there won't be more than 500 bombs in any test

genRandomGameWithoutOpened :: FieldChars -> Gen GameState
genRandomGameWithoutOpened field = do
    flags <- (genInt 0 600)
    genGameHelper field flags
        where
            genGameHelper field flagsLeft = do
                if (flagsLeft == 0) then do
                    return GameState { flags = Set.empty, opened = Set.empty, status = Ongoing }
                else do
                    game <- (genGameHelper field (flagsLeft - 1))
                    x <- (genInt 0 ((rows field) - 1))
                    y <- (genInt 0 ((cols field) - 1))
                    return game { flags = Set.insert (Cell x y) (flags game) }

prop_noOpenedBombsOrFlags :: Property
prop_noOpenedBombsOrFlags = property $ do
    rndList <- forAll $ getRndList
    rows <- forAll $ (genInt 10 20)
    cols <- forAll $ (genInt 10 20)
    bombs <- forAll $ (genInt 20 33)
    let field = fst (generateField rows cols bombs rndList)
    game <- forAll $ (genRandomGameWithoutOpened field)
    x <- forAll $ (genInt 0 (rows - 1))
    y <- forAll $ (genInt 0 (cols - 1))
    let opened = (openAreaWhenClick field game (Cell x y))
    assert (checkNoBombsAndFlags field game opened)
        where
            checkNoBombsAndFlags field game opened = 
                (Set.size (Set.filter (\x -> (Set.member x (bombs field)) || (Set.member x (flags game))) opened) == 0)

props :: [TestTree]
props =
  [ testProperty "No bombs or flag cells are opened with openAreaWhenClick" prop_noOpenedBombsOrFlags
  ]