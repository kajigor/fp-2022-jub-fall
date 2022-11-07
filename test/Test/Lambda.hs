module Test.Lambda where
import Test.Tasty.HUnit (assertBool, (@?=))

import Lambda

unit_show :: IO ()
unit_show = do
    -- \x.(x) == \x.x
    assertBool "abs: yeet () 1" (show (Abs "x" (Var "x")) == "\\x.x")

    -- \x.(\y.z) == \x.\y.z
    assertBool "abs: yeet () 2" (show (Abs "x" (Abs "y" (Var "z"))) == "\\x.\\y.z")

    -- \x.(x \y.z) == \x.x \y.z
    assertBool "abs: yeet () 3" (show (Abs "x" (App (Var "x") (Abs "y" (Var "z")))) == "\\x.x \\y.z")

    -- (x y) z == x y z
    assertBool "app: yeet ()" (show (App (App (Var "x") (Var "y")) (Var "z")) == "x y z")

    -- (\x.\y.x) y /= \x.\y.x y
    assertBool "app: save () 1" (show (App (Abs "x" (Abs "y" (Var "x"))) (Var "y")) == "(\\x.\\y.x) y")

    -- x (y z) /= x y z
    assertBool "app: save () 2" (show (App (Var "x") (App (Var "y") (Var "z"))) == "x (y z)")


