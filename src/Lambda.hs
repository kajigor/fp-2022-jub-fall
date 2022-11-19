{-# LANGUAGE FlexibleInstances #-}
module Lambda where

import Control.Monad.Error (Error(strMsg))

import Data.List
import Data.Maybe
import Data.Char
import Control.Concurrent (yield)


-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda a = Var a
              | App (Lambda a) (Lambda a)
              | Abs a (Lambda a)

isVar::Lambda a -> Bool
isVar (Var _) = True
isVar _ = False

isApp::Lambda a -> Bool
isApp App{} = True
isApp _ = False

isAbs::Lambda a -> Bool
isAbs Abs{} = True
isAbs _ = False

wrapIf:: Bool -> String -> String
wrapIf True s = "(" ++ s ++ ")"
wrapIf False s = s

-- Красивая печать без лишних скобок.
instance {-# OVERLAPS #-} Show (Lambda String) where
  show (Var str) = str
  show (App x y) = wrapIf (isAbs x) (show x) ++ " " ++ wrapIf (isAbs y || isApp y) (show y)
  show (Abs x y) = "\\" ++ x ++ "." ++ show y

instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
  show = undefined

-- Выберите подходящий тип для подстановок.
data Subst a

-- Проверка термов на альфа-эквивалентность.
alphaEq :: Eq a => Lambda a -> Lambda a -> Bool
alphaEq = undefined

-- Capture-avoiding substitution.
cas :: Lambda a -> Subst a -> Lambda a
cas = undefined

-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: Strategy -> Lambda a -> Lambda a
eval = undefined

-- ДеБрауновское представление лямбда-термов
data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn

isVarDB::DeBruijn -> Bool
isVarDB VarDB{} = True
isVarDB _ = False

isAppDB::DeBruijn -> Bool
isAppDB AppDB{} = True
isAppDB _ = False

isAbsDB::DeBruijn -> Bool
isAbsDB AbsDB{} = True
isAbsDB _ = False

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show (VarDB num) = show num
  show (AppDB x y) = wrapIf (isAbsDB x) (show x) ++ " " ++ wrapIf (isAbsDB y || isAppDB y) (show y)
  show (AbsDB x) = "\\ " ++ show x

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Eq a => Lambda a -> DeBruijn
toDeBruijn = convert []
  where
    convert acc (Var str) = VarDB (fromJust (elemIndex str acc))
    convert acc (App x y) = AppDB (convert acc x) (convert acc y)
    convert acc (Abs x y) = AbsDB (convert (x:acc) y)


-- Lambdas
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
three =  Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))

--  four ≡ λf.λx.f (f (f (f x)))
four =  Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))

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

