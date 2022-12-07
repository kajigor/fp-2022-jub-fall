module Test.MinesweeperTest where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Minesweeper
import Data.Set as Set

field1 :: FieldChars
field1 = 
    FieldChars {
        rows = 4,
        cols = 4,
        bombs = Set.fromList [(Cell 0 1), (Cell 0 2), (Cell 0 3), (Cell 2 0)],
        counts = [[1, 2, 3, 2], [2, 3, 3, 2], [1, 1, 0, 0], [1, 1, 0, 0]]
    }

game1begin :: GameState
game1begin =
    GameState {
        opened = Set.empty,
        flags = Set.empty,
        status = Ongoing
    }

game1goes1 :: GameState
game1goes1 = 
    GameState {
        opened = Set.fromList [(Cell 0 0)],
        flags = Set.fromList [(Cell 1 0)],
        status = Ongoing
    }

game1goes2 :: GameState
game1goes2 = 
    GameState {
        opened = Set.fromList [(Cell 0 0), (Cell 1 1), (Cell 1 2), (Cell 1 3), (Cell 2 1), (Cell 2 2), (Cell 2 3), (Cell 3 1), (Cell 3 2), (Cell 3 3)],
        flags = Set.fromList [(Cell 1 0)],
        status = Ongoing
    }

game1fail :: GameState
game1fail = 
    GameState {
        opened = Set.fromList [(Cell 0 0), (Cell 1 1), (Cell 1 2), (Cell 1 3), (Cell 2 1), (Cell 2 2), (Cell 2 3), (Cell 3 1), (Cell 3 2), (Cell 3 3), (Cell 0 1)],
        flags = Set.fromList [(Cell 1 0)],
        status = Lose
    }

game1changeflag :: GameState
game1changeflag = 
    GameState {
        opened = Set.fromList [(Cell 0 0), (Cell 1 1), (Cell 1 2), (Cell 1 3), (Cell 2 1), (Cell 2 2), (Cell 2 3), (Cell 3 1), (Cell 3 2), (Cell 3 3)],
        flags = Set.fromList [(Cell 0 1)],
        status = Ongoing
    }

game1win :: GameState
game1win = 
    GameState {
        opened = Set.fromList [(Cell 0 0), (Cell 1 1), (Cell 1 2), (Cell 1 3), (Cell 2 1), (Cell 2 2), (Cell 2 3), (Cell 3 1), (Cell 3 2), (Cell 3 3), (Cell 1 0), (Cell 3 0)],
        flags = Set.fromList [(Cell 0 1)],
        status = Win
    }

unit_generateField = do
    let generated = (generateField 2 5 5 [1, 2, 7, 0, 3, 5, 5, 5])
    (snd generated) @?= [5, 5, 5]
    (bombs (fst generated)) @?= Set.fromList [(Cell 0 1), (Cell 0 3), (Cell 1 4), (Cell 0 0), (Cell 1 1)]
    (counts (fst generated)) @?= [[3, 3, 3, 2, 2], [3, 3, 3, 2, 2]]
    (rows (fst generated)) @?= 2
    (cols (fst generated)) @?= 5

unit_countNeighbour = do
    countNeighbour 10 10 (Cell 2 2) (\cell -> (x cell) + (y cell) <= 4) @?= 6
    countNeighbour 10 10 (Cell 0 1) (\cell -> True) @?= 6
    countNeighbour 2 3 (Cell 1 2) (\cell -> True) @?= 4

unit_countCounts = do
    countCounts 4 4 (bombs field1) @?= (counts field1)

unit_isFlagOnOpening = do
    isFlagOnOpening field1 game1goes1 (Cell 0 0) @?= False
    isFlagOnOpening field1 game1goes1 (Cell 1 0) @?= True

unit_isBombActivated = do
    isBombActivated field1 game1goes1 (Cell 3 3) @?= False
    isBombActivated field1 game1goes1 (Cell 2 0) @?= True

unit_isCellOpened= do
    isCellOpened field1 game1goes2 (Cell 1 0) @?= False
    isCellOpened field1 game1goes2 (Cell 3 2) @?= True

unit_isGameFinished = do
    isGameFinished field1 game1goes1 @?= False
    isGameFinished field1 game1win @?= True

unit_openAreaWhenClick = do
    openAreaWhenClick field1 game1begin (Cell 0 0) @?= Set.fromList [(Cell 0 0)]
    openAreaWhenClick field1 game1begin (Cell 3 2) @?= Set.fromList [(Cell 1 1), (Cell 1 2), (Cell 1 3), (Cell 2 1), (Cell 2 2), (Cell 2 3), (Cell 3 1), (Cell 3 2), (Cell 3 3)]

unit_doActions = do
    (fst doActionOnFlag field1 (doActionOnOpen field1 game1begin (Cell 0 0)) (Cell 1 0)) @?= game1goes1
    (fst doActionOnOpen field1 game1goes1 (Cell 2 2)) @?= game1goes2
    (fst doActionOnOpen field1 game1goes2 (Cell 1 0)) @?= game1goes2
    (fst doActionOnOpen field1 game1goes2 (Cell 0 1)) @?= game1fail
    (fst doActionOnFlag field1 (doActionOnFlag field1 game1goes2 (Cell 1 0)) (Cell 0 1)) @?= game1changeflag
    (fst doActionOnOpen field1 (doActionOnOpen field1 game1changeflag (Cell 1 0)) (Cell 3 0)) @?= game1win