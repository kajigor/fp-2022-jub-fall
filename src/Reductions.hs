{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Reductions where
import Lambda
import Control.Monad.State
import Data.Maybe
import qualified Data.Set as Set

-- Выберите подходящий тип для подстановок.
data Subst = Subst String Lambda

-- Capture-avoiding substitution.
cas :: Lambda -> Subst -> Lambda
cas (Var y) (Subst x m) =
  if x == y
    then m
    else Var y
cas (App x y) s = App (cas x s) (cas y s)
cas (Abs y expr) (Subst x m)
  | y == x = Abs y expr
  | y `Set.notMember` fvm = Abs y (expr `cas` Subst x m)
  | otherwise = Abs z (expr `cas` Subst y (Var z) `cas` Subst x m)
  where
    fvm = freeVariables m
    z = fresh (x `Set.insert` (getVariables expr `Set.union` getVariables m))

-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder
  deriving Show

isWeak :: Strategy -> Bool
isWeak CallByValue = True
isWeak CallByName = True
isWeak _ = False

-- Выполняет CAS на верхнем уровне, если можно, и снова запускает eval на результате
evalApp :: Strategy -> Lambda -> Lambda -> StateT Int Maybe Lambda
evalApp strategy (Abs x y) z = evalBounded strategy (y `cas` Subst x z)
evalApp strategy x y = do
  fuel <- get
  if fuel <= 0
    then lift Nothing
    else put (fuel - 1)
    
  x' <- evalBounded strategy x
  y' <- evalBounded strategy y
  return $ App x' y'

-- Интерпретатор лямбда термов, учитывающий стратегию.
evalBounded :: Strategy -> Lambda -> StateT Int Maybe Lambda
evalBounded _ (Var x) = return $ Var x

evalBounded strategy (Abs x y)
  | isWeak strategy = return $ Abs x y
  | otherwise = do
      y' <- evalBounded strategy y
      return $ Abs x y'

evalBounded CallByName (App x y) = do
  x' <- evalBounded CallByName x
  evalApp CallByName x' y

evalBounded NormalOrder (App x y) = do
  x' <- evalBounded CallByName x
  evalApp NormalOrder x' y

evalBounded CallByValue (App x y) = do
  x' <- evalBounded CallByValue x
  y' <- evalBounded CallByValue y
  evalApp CallByValue x' y'

evalBounded ApplicativeOrder (App x y) = do
  x' <- evalBounded ApplicativeOrder x
  y' <- evalBounded ApplicativeOrder y
  evalApp ApplicativeOrder x' y'

evalMaybe :: Strategy -> Lambda -> Maybe Int -> Maybe Lambda
evalMaybe strategy expression limit = evalStateT (evalBounded strategy expression) (fromMaybe maxBound limit)

eval :: Strategy -> Lambda -> Lambda
eval strategy expression = fromJust (evalMaybe strategy expression Nothing)