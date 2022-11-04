module Main (main) where
import Expr
import Text.Read

getAmountOfExpr :: IO Int
getAmountOfExpr = do
    putStrLn "Please, enter the amount of expressions you want to see: "
    answer <- getLine
    let amount = readMaybe answer
    case amount of
        Just val -> do
            if val <= 0
            then do
                putStrLn "The amount must be a positive integer."
                getAmountOfExpr
            else return val
        Nothing -> do
            putStrLn "The amount must be a positive integer."
            getAmountOfExpr

getCommand :: IO String
getCommand = do
    putStrLn "Choose a type of an expression you want to see:"
    putStrLn "    DivisionByZero"
    putStrLn "    LogOfZero"
    putStrLn "    LogOfNegativeNumber"
    putStrLn "    RootNegativeNumber"
    putStrLn "    RootZeroDegree"
    putStrLn "    Number (enter an Integer or a Double)"
    answer <- getLine
    return answer

start :: Int -> IO ()
start amount = do
    input <- getCommand
    case input of
        "DivisionByZero" -> putStrLn $ show (take amount (generateExprByResult (Left DivisionByZero)))
        "LogOfZero" -> putStrLn $ show (take amount (generateExprByResult (Left LogOfZero)))
        "LogOfNegativeNumber" -> putStrLn $ show (take amount (generateExprByResult (Left LogOfNegativeNumber)))
        "RootNegativeNumber" -> putStrLn $ show (take amount (generateExprByResult (Left RootNegativeNumber)))
        "RootZeroDegree" -> putStrLn $ show (take amount (generateExprByResult (Left RootZeroDegree)))
        otherwise -> do
            let number = readMaybe input
            case number of
                Just val -> putStrLn $ show (take amount (generateExprByResult (Right val)))
                Nothing -> do
                    putStrLn "Your input is incorrect."
                    start amount


main :: IO ()
main = do
    amount <- getAmountOfExpr
    start amount

