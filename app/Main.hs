module Main (main) where

import Minesweeper
import System.Random

readThreeInts :: String -> IO [Int]
readThreeInts message = do
    putStrLn message
    ints <- fmap (map read.words) getLine
    if length ints == 3 then return ints
    else do
        putStrLn "Too many or not enough numbers inputted, try again."
        readThreeInts message

getCustomParams :: IO [Int]
getCustomParams = do
    paramsLst <- readThreeInts "Input the number of rows, cols and bombs in your field separated by space:"
    let rows = paramsLst!!0
    let cols = paramsLst!!1
    let bombs = paramsLst!!2
    if (rows < 4 || cols < 4 || rows > 99 || cols > 99 || bombs > (rows * cols) `div` 3) then do
        putStrLn "Not satisfied: 4 <= rows <= 99, 4 <= cols <= 99, bombs <= (rows * cols) / 3, please try again"
        getCustomParams
    else
        return paramsLst

getFieldParams :: IO [Int]
getFieldParams = do
    putStrLn "Do you want to play with custom field? (y/n)"
    wish <- getLine
    if (wish == "y" || wish == "Y") then getCustomParams
    else return [8, 8, 10]

getCellTurn :: IO [Int]
getCellTurn = do
    lst <- readThreeInts "Input your move in format 0 x y if you want to open the cell (x, y) and in format 1 x y if you want to put flag on the cell (x, y):"
    return lst

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
        turn <- getCellTurn
        let flagOpen = turn!!0
        let x = turn!!1
        let y = turn!!2
        if (not (isCellInBounds (rows field) (cols field) (Cell x y))) then do
            putStrLn "Cell coordinates out of bound, try again."
            doGameCycle field game
        else if (flagOpen == 0) then do
            let newGamePair = doActionOnOpen field game (Cell x y)
            putStrLn (snd newGamePair)
            doGameCycle field (fst newGamePair)
        else do
            let newGamePair = doActionOnFlag field game (Cell x y)
            putStrLn (snd newGamePair)
            doGameCycle field (fst newGamePair)

doManyGamesCycle :: [Int] -> IO ()
doManyGamesCycle rndList = do
    paramsLst <- getFieldParams
    let rows = paramsLst!!0
    let cols = paramsLst!!1
    let bombs = paramsLst!!2
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