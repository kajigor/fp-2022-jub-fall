module Main (main) where

import Minesweeper
import System.Random

data Move = Flag { cell :: Cell } | Open { cell :: Cell }

readThreeInts :: String -> IO [Int]
readThreeInts message = do
    putStrLn message
    intsMaybe <- fmap (map read.words) getLine
    case intsMaybe of
        Nothing -> do
            putStrLn "You inputted not a number, try again."
            readThreeInts message
        Just ints -> 
            if length ints == 3 then return ints
            else do
                putStrLn "Too many or not enough numbers inputted, try again."
                readThreeInts message

getCustomParams :: IO [Int]
getCustomParams = do
    [rows, cols, bombs] <- readThreeInts "Input the number of rows, cols and bombs in your field separated by space:"
    if (rows < 4 || cols < 4 || rows > 99 || cols > 99 || bombs > (rows * cols) `div` 3) then do
        putStrLn "Not satisfied: 4 <= rows <= 99, 4 <= cols <= 99, bombs <= (rows * cols) / 3, please try again"
        getCustomParams
    else
        return [rows, cols, bombs]

getFieldParams :: IO [Int]
getFieldParams = do
    putStrLn "Do you want to play with custom field? (y/n)"
    wish <- getLine
    if (wish == "y" || wish == "Y") then getCustomParams
    else return [8, 8, 10]

getCellTurn :: IO Move
getCellTurn = do
    [flagOpen, x, y] <- readThreeInts "Input your move in format 0 x y if you want to open the cell (x, y) and in format 1 x y if you want to put flag on the cell (x, y):"
    if (flagOpen == 0) then do
        return Open (Cell x y)
    else do
        return Flag (Cell x y)

getLineWithFunction :: FieldChars -> (Int -> String) -> String
getLineWithFunction field func = getEmptyLineHelper 0 field func
    where
        getEmptyLineHelper x field func = 
            if (x == (cols field)) then ""
            else (func x) ++ (getEmptyLineHelper (x + 1) field func)

printGame :: FieldChars -> GameState -> IO ()
printGame field game = do
    putStrLn ("   " ++ getLineWithFunction field (\x -> if (x >= 10) then (show (x `div ` 10)) else " "))
    putStrLn ("   " ++ (getLineWithFunction field (\x -> (show (x `mod` 10)))))
    putStrLn ("   " ++ getLineWithFunction field (\x -> " "))
    printGameHelper field game 0 0 ""
    where 
        printGameHelper field game x y currRow = do
            if (x == (rows field)) then return ()
            else if (y == (cols field)) then do
                putStrLn currRow 
                printGameHelper field game (x + 1) 0 "" 
            else if (y == 0) then (printGameHelper field game x (y + 1) ((if (x >= 10) then (show (x `div ` 10)) else " ") ++ (show (x `mod` 10)) ++ " " ++ (getSymb field game x y)))
            else (printGameHelper field game x (y + 1) (currRow ++ (getSymb field game x y)))

doActionOnOpen :: FieldChars -> GameState -> Cell -> IO (GameState)
doActionOnOpen field game cell =
    if ((isFlagOnOpening field game cell) || (isCellOpened field game cell)) then do
        putStrLn "Can't open a cell with a flag or an already opened cell"
        return game
    else if (isBombActivated field game cell) then do
        putStrLn "Boom"
        return game { opened = (Set.insert cell (opened game)), status = Lose }
    else do
        putStrLn "Successfully opened a cell"
        return (checkAndFinishGame field (game { opened = Set.union (opened game) (openAreaWhenClick field game cell)}))

doActionOnFlag :: FieldChars -> GameState -> Cell -> IO (GameState)
doActionOnFlag field game cell = 
    if (isCellOpened field game cell) then do
        putStrLn "Can't put a flag on an opened cell"
        return game
    else if (isFlagOnOpening field game cell) then do
        putStrLn "Erased a flag"
        return game { flags = (Set.delete cell (flags game)) }
    else do
        putStrLn "Put a flag"
        return game { flags = (Set.insert cell (flags game)) }

doGameCycle :: FieldChars -> GameState -> IO (GameState)
doGameCycle field game = do
    printGame field game
    if (status game == Win) then do
        putStrLn "You won!"
        return game
    else if (status game == Lose) then do
        putStrLn "You lose..."
        return game
    else do
        move <- getCellTurn
        if (not (isCellInBounds (rows field) (cols field) (cell move))) then do
            putStrLn "Cell coordinates out of bound, try again."
            doGameCycle field game
        else if (move == Open) then do
            newGame <- doActionOnOpen field game (cell move)
            doGameCycle field newGame
        else do
            newGame <- doActionOnFlag field game (cell move)
            doGameCycle field newGame

doManyGamesCycle :: [Int] -> IO ()
doManyGamesCycle rndList = do
    [rows, cols, bombs] <- getFieldParams
    let fieldPair = generateField rows cols bombs rndList
    let field = fst fieldPair
    let newRndLst = snd fieldPair
    let game = gameBegin

    doGameCycle field game

    putStrLn "Do you want to play again? (y/n)"
    wish <- getLine
    if (wish == "y" || wish == "Y") then doManyGamesCycle newRndLst
    else return ()


main :: IO ()
main = do
    g <- newStdGen
    let rndList = (randomRs (0, 2147483647) g)
    doManyGamesCycle rndList
    return ()