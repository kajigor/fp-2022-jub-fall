{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lambda where

import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as Set

-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda a
  = Var a
  | App (Lambda a) (Lambda a)
  | Abs a (Lambda a)

-- true ≡ λx.λy.x
true = Abs "x" (Abs "y" (Var "x"))

-- false ≡ λx.λy.y
false = Abs "x" (Abs "y" (Var "y"))

-- and ≡ λp.λq.p q p
and = Abs "p" (Abs "q" (App (App (Var "p") (Var "q")) (Var "p")))

-- or ≡ λp.λq.p p q
or = Abs "p" (Abs "q" (App (App (Var "p") (Var "p")) (Var "q")))

-- not ≡ λp.p FALSE TRUE
not = Abs "p" (App (App (Var "p") false) true)

-- ifThenElse ≡ λp.λa.λb.p a b
ifThenElse = Abs "p" (Abs "a" (Abs "b" (App (App (Var "p") (Var "a")) (Var "b"))))

-- zero ≡ λf.λx.x
zero = Abs "f" (Abs "x" (Var "x"))

-- one ≡ λf.λx.f x
one = Abs "f" (Abs "x" (App (Var "f") (Var "x")))

-- two ≡ λf.λx.f (f x)
two = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (Var "x"))))

-- three ≡ λf.λx.f (f (f x))
three = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))

--  four ≡ λf.λx.f (f (f (f x)))
four = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))

-- add ≡ λm.λn.λf.λx.m f (n f x)
add = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x"))))))

-- successor ≡ λn.λf.λx.f (n f x)
successor = Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))

-- add' ≡ λm.λn.m successor n
add' = Abs "m" (Abs "n" (App (App (Var "m") successor) (Var "n")))

-- mult ≡ λm.λn.λf.m (n f)
mult = Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f")))))

-- mult' ≡ λm.λn.m (add n) 0
mult' = Abs "m" (Abs "n" (App (App (Var "m") (App add (Var "n"))) zero))

class ShowProperly a where
  repr :: a -> String

instance {-# OVERLAPPABLE #-} Show a => ShowProperly a where
  repr = show

instance {-# OVERLAPS #-} ShowProperly String where
  repr = id

condWrap :: Bool -> String -> String
condWrap True s = "(" ++ s ++ ")"
condWrap False s = s

isApp :: Lambda a -> Bool
isApp e = case e of
  App _ _ -> True
  _ -> False

isAbs :: Lambda a -> Bool
isAbs e = case e of
  Abs _ _ -> True
  _ -> False

-- Красивая печать без лишних скобок.
instance ShowProperly a => Show (Lambda a) where
  show expr = case expr of
    Var s -> repr s
    App x y ->
      condWrap (isAbs x) (show x) ++ " "
        ++ condWrap (isApp y || isAbs y) (show y)
    Abs x y -> "\\" ++ repr x ++ "." ++ show y

class Ord a => Fresh a where
  candidates :: [a]
  candidates = undefined

  fresh :: Set.Set a -> a
  fresh used = head $ filter isFree candidates
    where
      isFree x = x `Set.notMember` used

instance Fresh String where
  candidates = [f n | n <- [0 ..]]
    where
      alpha = 26
      f n
        | n < alpha = [chr (n + ord 'a')]
        | otherwise = f (n `div` alpha) ++ f (n `rem` alpha)

-- Выберите подходящий тип для подстановок.
data Subst a = Subst a (Lambda a)

-- Проверка термов на альфа-эквивалентность.
alphaEq :: Eq a => Lambda a -> Lambda a -> Bool
alphaEq = (==) `on` (show . toDeBruijn)

freeVariables :: Ord a => Lambda a -> Set.Set a
freeVariables (Var x) = Set.singleton x
freeVariables (App x y) = freeVariables x `Set.union` freeVariables y
freeVariables (Abs x y) = freeVariables y `Set.difference` Set.singleton x

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
    z = fresh (x `Set.insert` fvm)

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

isAppDB :: DeBruijn -> Bool
isAppDB e = case e of
  AppDB _ _ -> True
  _ -> False

isAbsDB :: DeBruijn -> Bool
isAbsDB e = case e of
  AbsDB _ -> True
  _ -> False

-- ДеБрауновское представление лямбда-термов
data DeBruijn
  = VarDB Int
  | AppDB DeBruijn DeBruijn
  | AbsDB DeBruijn

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show expr = case expr of
    VarDB s -> show s
    AppDB x y ->
      condWrap (isAbsDB x) (show x) ++ " "
        ++ condWrap (isAppDB y || isAbsDB y) (show y)
    AbsDB y -> "\\ " ++ show y

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Eq a => Lambda a -> DeBruijn
toDeBruijn = fromJust . f []
  where
    f s (Var x) = do
      i <- elemIndex x s
      return $ VarDB (i + 1)
    f s (App x y) = do
      x' <- f s x
      y' <- f s y
      return $ AppDB x' y'
    f s (Abs x y) = do
      y' <- f (x : s) y
      return $ AbsDB y'

-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: Fresh a =>  DeBruijn -> Lambda a
fromDeBruijn = f []
  where 
    f :: Fresh a => [a] -> DeBruijn -> Lambda a
    f stack (VarDB x) = Var $ fromJust $ listToMaybe $ drop (x - 1) stack
    f stack (AppDB x y) = App (f stack x) (f stack y)
    f stack (AbsDB y) = Abs x (f (x:stack) y)
        where x = fresh $ Set.fromList stack
        