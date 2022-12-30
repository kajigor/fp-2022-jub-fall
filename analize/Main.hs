module Main where

import Integration
import Control.Monad

main :: IO ()
main = do
    forM_ [(\x -> 2 * x + 1 / sqrt (x + 1/16), 0.0, 1.5, "2*x+1/sqrt(x+1/16)"), (\x -> cos (x / 2) * x, 2.0, 5.0, "cos(x/2)*x"), (\x -> x*x*x*x*x-7*x*x*x*x+4*x*x*x-20*x*x+17*x+3, -13.0, 20.0, "x^5-7*x^4+4*x^3-20*x^2+17*x+3"), (\x -> exp (3 * x / 5) + log (x / 4) / x, 13.0, 25.0, "e^(3*x/5)+log(x/4)/x")] (\(f, a, b, fName) -> do
        putStrLn fName
        forM_ [1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6] (\eps -> do
            print eps
            forM_ [(evalIntegralMidPointWithIndex, "Middle Point Method"), (evalIntegralTrapezoidWithIndex, "Trapezoid Method"), (evalIntegralSimpsonWithIndex, "Simpson Method")] (\(evalInt, name) -> do
                case evalInt f a b eps of
                    Right (n, _) -> putStrLn (name ++ ": " ++ show n)
                    _ -> putStrLn (name ++ ": has not ended successfully"))))