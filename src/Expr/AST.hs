module Expr.AST where

import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Trans.State
import Control.Monad.Trans.Class


data Expr = BinOp Op Expr Expr
          | Number Int
          | Ident String
          deriving (Show, Eq)

data Op = Plus
        | Minus
        | Mult
        | Div
        | Pow
        deriving (Show, Eq)

vars :: Expr -> Set.Set String
vars (BinOp _ l r) =
  Set.union (vars l) (vars r)
vars (Ident v) =
  Set.singleton v
vars _ =
  Set.empty

runEval expr varMap =
  evalState (eval expr) varMap

runEvalSafe :: Expr -> [(String, Int)] -> Either String Int
runEvalSafe expr varMap =
  evalStateT (evalSafe expr) $ Map.fromList varMap

eval :: Expr -> State (Map.Map String Int) Int
eval (Number x) = return x
eval (Ident v) = do
  blah <- put Map.empty -- put >>= \blah -> ....
  varMap <- get -- get >>= \varMap -> ...
  case Map.lookup v varMap of
    Just r -> return r
    Nothing -> return 42

eval (BinOp op x y) = do
    x <- eval x
    y <- eval y
    return $ fromOp op x y
  where
    fromOp Plus = (+)
    fromOp Minus = (-)
    fromOp Mult = (*)
    fromOp Div = div
    fromOp Pow = (^)

evalSafe :: Expr -> StateT (Map.Map String Int) (Either String) Int
evalSafe (Number x) = return x
evalSafe (Ident v) = do
  varMap <- get
  case Map.lookup v varMap of
    Just r -> return r
    Nothing -> lift $ Left $ "Undefined variable " ++ v
evalSafe (BinOp op x y) = do
    x <- evalSafe x
    y <- evalSafe y
    return $ fromOp op x y
  where
    fromOp Plus = (+)
    fromOp Minus = (-)
    fromOp Mult = (*)
    fromOp Div = div
    fromOp Pow = (^)

-- x + 1 (, where x == 452)



-- eval :: Expr -> Int
-- eval (Number x) = x
-- eval (BinOp op x y) =
--     fromOp op (eval x) (eval y)
--   where
--     fromOp Plus = (+)
--     fromOp Minus = \x y -> x - y
--     fromOp Mult = (*)
--     fromOp Div = div
--     fromOp Pow = (^)

printOp :: Op -> String
printOp Plus = "+"
printOp Minus = "-"
printOp Mult = "*"
printOp Div = "/"
printOp Pow = "^"

printInfix :: Expr -> String
printInfix (BinOp op l r) = printf "(%s %s %s)" (printInfix l) (printOp op) (printInfix r)
printInfix (Number n) = show n

printPrefix :: Expr -> String
printPrefix (BinOp op l r) = printf "%s %s %s" (printOp op) (printPrefix l) (printPrefix r)
printPrefix (Number n) = show n

-- parser (printer expr) == expr
