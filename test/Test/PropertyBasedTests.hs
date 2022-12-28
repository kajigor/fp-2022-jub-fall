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

getRndList :: Int -> Gen [Int]
getRndList len = Gen.list (Range.constant 50 50) (genInt 0 2147483647)

-- It is not guaranteed that the game will contain EXACTLY flagsLeft flags
-- Though it is guaranteed that the game will contain AT MOST flagsLeft flags
genRandomGameWithoutOpened :: FieldChars -> Int -> Gen GameState
genRandomGameWithoutOpened field flagsLeft = do
    if (flagsLeft == 0) then do
        return GameState { flags = Set.empty, opened = Set.empty, status = Ongoing }
    else do
        game <- (genRandomGameWithoutOpened field (flagsLeft - 1))
        x <- (genInt 0 ((rows field) - 1))
        y <- (genInt 0 ((cols field) - 1))
        return game { flags = Set.insert (Cell x y) (flags game) }

getRandomCell :: FieldChars -> Gen Cell
getRandomCell field = do
    x <- (genInt 0 ((rows field) - 1))
    y <- (genInt 0 ((cols field) - 1))
    return (Cell x y)

cellIsNotFlagOrBomb :: FieldChars -> GameState -> Cell -> Bool
cellIsNotFlagOrBomb field game cell = ((not $ isFlagOnOpening field game cell) && (not $ isBombActivated field game cell))

getNonBombAndNonFlag :: FieldChars -> GameState -> Gen Cell 
getNonBombAndNonFlag field game = do
    cell <- getRandomCell field
    if (not $ cellIsNotFlagOrBomb field game cell) then getNonBombAndNonFlag field game
    else do
        return cell

getRandomField :: Gen FieldChars
getRandomField = do
    rows <- (genInt 10 20)
    cols <- (genInt 10 20)
    bombs <- (genInt 20 33)
    rndList <- getRndList (bombs + 10)
    return $ fst (generateField rows cols bombs rndList)

openSomeSafely :: FieldChars -> GameState -> Int -> Gen GameState
openSomeSafely field game triesOpen = do
    if (triesOpen == 0) then do
        return game
    else do
        cell <- getRandomCell field
        if (cellIsNotFlagOrBomb field game cell) then
            openSomeSafely field game { opened = Set.union (opened game) (openAreaWhenClick field game cell) } (triesOpen - 1)
        else
            openSomeSafely field game (triesOpen - 1)

prop_noOpenedBombsOrFlags :: Property
prop_noOpenedBombsOrFlags = property $ do
    field <- forAll $ getRandomField
    flags <- forAll $ (genInt 0 ((rows field) * (cols field)))
    game <- forAll $ (genRandomGameWithoutOpened field flags)
    cell <- forAll $ (getNonBombAndNonFlag field game)
    let opened = (openAreaWhenClick field game cell)
    assert (checkNoBombsAndFlags field game opened)
        where
            checkNoBombsAndFlags field game opened = 
                (Set.size (Set.filter (\x -> (Set.member x (bombs field)) || (Set.member x (flags game))) opened) == 0)

prop_countNeighbourCellsAlwaysInBounds :: Property
prop_countNeighbourCellsAlwaysInBounds = property $ do
    field <- forAll $ getRandomField
    cell <- forAll $ (getRandomCell field)
    let cntAll = countNeighbour (rows field) (cols field) cell (\x -> True)
    assert (0 <= cntAll && cntAll <= 9)

prop_noLoseOrWinWhenPutFlag :: Property
prop_noLoseOrWinWhenPutFlag = property $ do
    field <- forAll $ getRandomField
    flags <- forAll $ (genInt 0 ((rows field) * (cols field) `div` 3))
    game <- forAll $ (genRandomGameWithoutOpened field flags)
    triesOpen <- forAll $ (genInt 0 ((rows field) * (cols field) `div` 5))
    openedGame <- forAll $ (openSomeSafely field game triesOpen)
    
    cell <- forAll $ getRandomCell field
    let gameStatus = status $ fst (doActionOnFlag field openedGame cell)
    assert (gameStatus /= Lose && gameStatus /= Win)

prop_cellByNumberInBoundsAndNotBomb :: Property 
prop_cellByNumberInBoundsAndNotBomb = property $ do
    field <- forAll $ getRandomField
    number <- forAll $ (genInt 0 ((rows field) * (cols field) - 1 - (Set.size $ bombs field)))
    let cell = getCellByNumber number (bombs field) (cols field)
    assert ((isCellInBounds (rows field) (cols field) cell) && (Set.notMember cell (bombs field)))

props :: [TestTree]
props =
  [ testProperty "No bombs or flag cells are opened with openAreaWhenClick" prop_noOpenedBombsOrFlags
  , testProperty "Count neighbour does not return anything that is greater than 9 or less than 0" prop_countNeighbourCellsAlwaysInBounds
  , testProperty "Can't lose or win when putting up a flag" prop_noLoseOrWinWhenPutFlag
  , testProperty "Cell found by correct coordinates is always within bounds of the field and is not a bomb" prop_cellByNumberInBoundsAndNotBomb
  ]