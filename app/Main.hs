module Main (main) where

import Minesweeper
import System.Random
import Text.Read

data MoveType = Open | Flag deriving Eq

data Move = Move { 
    moveType :: MoveType,
    cell :: Cell
}

checkAnyIsNothing :: [Maybe Int] -> Bool
checkAnyIsNothing [] = False
checkAnyIsNothing (x : lst) =
    case x of
        Nothing -> True
        Just xInt -> checkAnyIsNothing lst

makeAListInt :: [Maybe Int] -> [Int]
makeAListInt [] = []
makeAListInt (x : lst) =
    case x of 
        Nothing -> [] -- can't happen when calling in transformListOfMaybeToMaybe
        Just xInt -> (xInt : (makeAListInt lst))

transformListOfMaybeToMaybe :: [Maybe Int] -> Maybe [Int]
transformListOfMaybeToMaybe lst = 
    if (checkAnyIsNothing lst) then Nothing
    else Just (makeAListInt lst)
    
readKInts :: Int -> String -> IO [Int]
readKInts cntNumbers message = do
    putStrLn message
    intsMaybe <- fmap (map readMaybe.words) getLine
    let intsMaybeList = transformListOfMaybeToMaybe intsMaybe
    case intsMaybeList of
        Nothing -> do
            putStrLn "You inputted not a number, try again."
            readKInts cntNumbers message
        Just ints -> do
            if (length ints == cntNumbers) then return ints
            else if (length ints < cntNumbers) then do
                putStrLn "Not enough numbers inputted, try again."
                readKInts cntNumbers message
            else do
                putStrLn "Too many numbers inputted, try again."
                readKInts cntNumbers message

getCustomParams :: IO [Int]
getCustomParams = do
    [rows, cols, bombs] <- readKInts 3 "Input the number of rows, cols and bombs in your field separated by space:"
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

getCellTurn :: FieldChars -> IO Move
getCellTurn field = do
    [flagOpen, x, y] <- readKInts 3 "Input your move in format 0 x y if you want to open the cell (x, y) and in format 1 x y if you want to put flag on the cell (x, y):"
    if (not (isCellInBounds (rows field) (cols field) (Cell x y))) then do
        putStrLn "Cell coordinates out of bound, try again."
        getCellTurn field
    else if (flagOpen == 0) then do
        return $ Move Open (Cell x y)
    else if (flagOpen == 1) then do
        return $ Move Flag (Cell x y)
    else do
        putStrLn "Expected 0 or 1 as the first argument, try again"
        getCellTurn field

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
        move <- getCellTurn field
        if ((moveType move) == Open) then do
            let newGamePair = doActionOnOpen field game (cell move)
            putStrLn (snd newGamePair)
            doGameCycle field (fst newGamePair)
        else do
            let newGamePair = doActionOnFlag field game (cell move)
            putStrLn (snd newGamePair)
            doGameCycle field (fst newGamePair)

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