{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Reductions where
import Lambda
import Data.Function
import qualified Data.Set as Set


-- Выберите подходящий тип для подстановок.
data Subst a = Subst a (Lambda a)

-- Capture-avoiding substitution.
cas :: Fresh a => Lambda a -> Subst a -> Lambda a
cas (Var y) (Subst x m) =
  if x == y
    then m
    else Var y
cas (App x y) s = App (cas x s) (cas y s)
cas (Abs y expr) (Subst x m)
  | (y == x) = Abs y expr
  | (y `Set.notMember` fvm) = Abs y (expr `cas` (Subst x m))
  | otherwise = Abs z (expr `cas` (Subst y (Var z)) `cas` (Subst x m))
  where
    fvm = freeVariables m
    z = fresh (x `Set.insert` (getVariables expr `Set.union` getVariables m))

-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

isWeak :: Strategy -> Bool
isWeak CallByValue = True
isWeak CallByName = True
isWeak _ = False

-- Выполняет CAS на верхнем уровне, если можно, и снова запускает eval на результате
evalApp :: Fresh a => Strategy -> Lambda a -> Lambda a -> Lambda a
evalApp strategy (Abs x y) z = eval strategy (y `cas` Subst x z)
evalApp strategy x y = App (eval strategy x) (eval strategy y)

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: Fresh a => Strategy -> Lambda a -> Lambda a
eval _ (Var x) = Var x
eval strategy (Abs x y)
  | isWeak strategy = Abs x y
  | otherwise = Abs x (eval strategy y)
eval CallByName (App x y) = evalApp CallByName (eval CallByName x) y
eval NormalOrder (App x y) = evalApp NormalOrder (eval CallByName x) y
eval CallByValue (App x y) = evalApp CallByValue (eval CallByValue x) (eval CallByValue y)
eval ApplicativeOrder (App x y) = evalApp ApplicativeOrder (eval ApplicativeOrder x) (eval ApplicativeOrder y)