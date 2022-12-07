module Minesweeper where

import System.Random
import qualified Data.Set as Set

data Cell = Cell {
    x :: Int,
    y :: Int
} deriving (Ord, Eq, Show)

addCell :: Cell -> Cell -> Cell
addCell cell1 cell2 = Cell ((x cell1) + (x cell2)) ((y cell1) + (y cell2))

data GameStatus = Ongoing | Win | Lose deriving (Show, Eq)

data GameState = GameState {
    flags :: Set.Set (Cell),
    opened :: Set.Set (Cell),
    status :: GameStatus
} deriving (Show, Eq)

gameBegin :: GameState
gameBegin =
    GameState {
        opened = Set.empty,
        flags = Set.empty,
        status = Ongoing
    }

data FieldChars = FieldChars {
    rows :: Int,
    cols :: Int,
    bombs :: Set.Set (Cell),
    counts :: [[Int]]
} deriving Show

getCellByNumber :: Int -> Int -> Int -> Set.Set (Cell) -> Int -> Cell
getCellByNumber number x y placedBombs cols =
    if (y == cols) then getCellByNumber number (x + 1) 0 placedBombs cols
    else if (number == 0) then Cell x y
    else if (Set.member (Cell x y) placedBombs) then getCellByNumber number x (y + 1) placedBombs cols
    else getCellByNumber (number - 1) x (y + 1) placedBombs cols 

generateField :: Int -> Int -> Int -> [Int] -> (FieldChars, [Int])
generateField rows cols cntBombs randList = generateFieldHelper rows cols cntBombs Set.empty randList
    where 
        generateFieldHelper rows cols cntBombs placedBombs (randomInt : randList) = do
            if ((Set.size placedBombs) == cntBombs) then (FieldChars rows cols placedBombs (countCounts rows cols placedBombs), randomInt : randList)
            else do
                let freeCells = (rows * cols - (Set.size placedBombs))
                generateFieldHelper rows cols cntBombs (Set.insert (getCellByNumber (randomInt `mod` freeCells) 0 0 placedBombs cols) placedBombs) randList

getSymb :: FieldChars -> GameState -> Int -> Int -> String
getSymb field game x y = 
    if (status game /= Ongoing && Set.member (Cell x y) (bombs field)) then "*"
    else if (Set.member (Cell x y) (flags game)) then "?"
    else if (Set.notMember (Cell x y) (opened game)) then "■"
    else if (((counts field)!!x)!!y > 0) then show (((counts field)!!x)!!y)
    else " "

isCellInBounds :: Int -> Int -> Cell -> Bool
isCellInBounds rows cols cell = ((x cell) >= 0 && (y cell) >= 0 && (x cell) < rows && (y cell) < cols)

countNeighbour :: Int -> Int -> Cell -> (Cell -> Bool) -> Int
countNeighbour rows cols cell predicate = countNeighbourHelper rows cols cell predicate (Cell (-1) (-1)) 
    where
        countNeighbourHelper rows cols cell predicate shift = do
            if (y shift == 2) then countNeighbourHelper rows cols cell predicate (Cell ((x shift) + 1) (-1))
            else if (x shift == 2) then 0
            else if ((isCellInBounds rows cols (addCell cell shift)) && (predicate (addCell cell shift))) then 1 + (countNeighbourHelper rows cols cell predicate (addCell shift (Cell 0 1))) 
            else countNeighbourHelper rows cols cell predicate (addCell shift (Cell 0 1))

countNeighbourBomb :: Int -> Int -> Cell -> Set.Set (Cell) -> Int
countNeighbourBomb rows cols cell placedBombs = countNeighbour rows cols cell (\x -> (Set.member x placedBombs))

countNeigbourFlag :: Int -> Int -> Cell -> GameState -> Int
countNeigbourFlag rows cols cell game = countNeighbour rows cols cell (\x -> (Set.member x (flags game)))

countCounts :: Int -> Int -> Set.Set (Cell) -> [[Int]]
countCounts rows cols placedBombs = countCountsHelper rows cols 0 0 placedBombs []
    where
        countCountsHelper rows cols x y placedBombs currRow = do
            if (x == rows) then []
            else if (y == cols) then currRow : (countCountsHelper rows cols (x + 1) 0 placedBombs [])
            else (countCountsHelper rows cols x (y + 1) placedBombs (currRow ++ [(countNeighbourBomb rows cols (Cell x y) placedBombs)]))

openAreaWhenClick :: FieldChars -> GameState -> Cell -> Set.Set (Cell)
openAreaWhenClick field game cell = fst (openAreaHelper field game cell Set.empty (Cell 0 0))
    where
        uniteFourSets set1 set2 set3 set4 = (Set.union (Set.union set1 set2) (Set.union set3 set4))
        openAreaHelper field game cell visited shift = do
            if ((not (isCellInBounds (rows field) (cols field) cell)) || (Set.member cell (flags game))) then (Set.empty, visited)
            else if (shift /= (Cell 0 0)) then openAreaHelper field game (addCell cell shift) visited (Cell 0 0)
            else if ((counts field)!! (x cell) !! (y cell) /= 0) then (Set.fromList [cell], visited)
            else if (Set.member cell visited) then (Set.empty, visited)
            else do
                let newVisited = (Set.insert cell visited)
                let ret1 = (openAreaHelper field game cell newVisited (Cell (-1) 0))
                let ret2 = (openAreaHelper field game cell (snd ret1) (Cell 1 0))
                let ret3 = (openAreaHelper field game cell (snd ret2) (Cell 0 (-1)))
                let ret4 = (openAreaHelper field game cell (snd ret3) (Cell 0 1))
                let ret5 = (openAreaHelper field game cell (snd ret4) (Cell (-1) (-1)))
                let ret6 = (openAreaHelper field game cell (snd ret5) (Cell (-1) 1))
                let ret7 = (openAreaHelper field game cell (snd ret6) (Cell 1 (-1)))
                let ret8 = (openAreaHelper field game cell (snd ret7) (Cell 1 1))
                (Set.union (Set.fromList [cell]) (Set.union (uniteFourSets (fst ret1) (fst ret2) (fst ret3) (fst ret4)) (uniteFourSets (fst ret5) (fst ret6) (fst ret7) (fst ret8))), (snd ret8))

isFlagOnOpening :: FieldChars -> GameState -> Cell -> Bool
isFlagOnOpening _ game cell = Set.member cell (flags game)

isCellOpened :: FieldChars -> GameState -> Cell -> Bool
isCellOpened _ game cell = Set.member cell (opened game)

isBombActivated :: FieldChars -> GameState -> Cell -> Bool
isBombActivated field _ cell = Set.member cell (bombs field)

isGameFinished :: FieldChars -> GameState -> Bool
isGameFinished field game = (Set.size (opened game) == (rows field) * (cols field) - (Set.size (bombs field)))

checkAndFinishGame :: FieldChars -> GameState -> GameState
checkAndFinishGame field game = 
    if (isGameFinished field game) then game { status = Win }
    else game

doActionOnOpen :: FieldChars -> GameState -> Cell -> (GameState, String)
doActionOnOpen field game cell =
    if ((isFlagOnOpening field game cell) || (isCellOpened field game cell)) then (game, "Can't open a cell with a flag or an already opened cell")
    else if (isBombActivated field game cell) then (game { opened = (Set.insert cell (opened game)), status = Lose }, "Boom")
    else (checkAndFinishGame field (game { opened = Set.union (opened game) (openAreaWhenClick field game cell)} ), "Successfully opened a cell")

doActionOnFlag :: FieldChars -> GameState -> Cell -> (GameState, String)
doActionOnFlag field game cell = 
    if (isCellOpened field game cell) then (game, "Can't put a flag on an opened cell")
    else if (isFlagOnOpening field game cell) then (game { flags = (Set.delete cell (flags game)) }, "Erased a flag")
    else (game { flags = (Set.insert cell (flags game)) }, "Put a flag")
