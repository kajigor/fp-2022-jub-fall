{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Lambda where

import Data.List (elemIndex)
import GHC.Float (stgWord32ToFloat)

-- Тип для лямбда-термов.
-- Параметризуем типом переменной.

data Lambda a
  = Var a
  | App (Lambda a) (Lambda a)
  | Abs a (Lambda a)
  deriving (Eq)

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
mult = Abs "m" (Abs "n" (App (Var "m") (App (Var "n") (Var "f"))))

-- mult' ≡ λm.λn.m (add n) 0
mult' = Abs "m" (Abs "n" (App (App (Var "m") (App add (Var "n"))) zero))

-- Красивая печать без лишних скобок.
instance {-# OVERLAPS #-} Show (Lambda String) where
  show (Var a) = a
  show (Abs a lamb) = "λ" ++ a ++ "." ++ show lamb
  show (App lamb1 (App b l)) = show lamb1 ++ "(" ++ show (App b l) ++ ")"
  show (App (App a l) lamb2) = show (App a l) ++ " " ++ show lamb2
  show (App lamb1 lamb2) = show lamb1 ++ " " ++ show lamb2

--instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
--show = undefined

-- Выберите подходящий тип для подстановок.
data Subst a = Sub (Lambda a) (Lambda a)

-- Проверка термов на альфа-эквивалентность.
alphaEq :: Eq a => Lambda a -> Lambda a -> Bool
alphaEq lamb1 lamb2 = toDeBruijn lamb1 [] [] == toDeBruijn lamb2 [] []

boundedVar :: Lambda a -> [a] -> [a]
boundedVar (Var a) bounded = bounded
boundedVar (Abs a lamb) bounded = a : bounded ++ boundedVar lamb []
boundedVar (App lamb1 lamb2) bounded = bounded ++ boundedVar lamb1 [] ++ boundedVar lamb2 []

-- Capture-avoiding substitution.
cas :: (Eq a) => Lambda a -> Subst a -> Lambda a
cas (Var a) (Sub (Var b) subLamb)
  | Var a == Var b = subLamb
  | otherwise = Var a
cas (App lamb1 lamb2) (Sub (Var b) subLamb) = App (cas lamb1 (Sub (Var b) subLamb)) (cas lamb2 (Sub (Var b) subLamb))
cas (Abs a lamb) (Sub (Var b) subLamb)
  | b == a = lamb
  | b /= a && a `elem` boundedVar subLamb [] = cas lamb (Sub (Var b) subLamb)
  | otherwise = undefined
cas _ _ = undefined

-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: Strategy -> Lambda a -> Lambda a
eval = undefined

-- ДеБрауновское представление лямбда-термов
data DeBruijn
  = VarDB Int
  | AbsDB DeBruijn
  | AppDB DeBruijn DeBruijn
  deriving (Eq)

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show (VarDB a) = show a
  show (AbsDB lamb) = "λ." ++ show lamb
  show (AppDB lamb1 (AppDB b l)) = show lamb1 ++ "(" ++ show (AppDB b l) ++ ")"
  show (AppDB (AppDB a l) lamb2) = show (AppDB a l) ++ " " ++ show lamb2
  show (AppDB lamb1 lamb2) = show lamb1 ++ " " ++ show lamb2

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
getVal :: Maybe Int -> Int
getVal (Just a) = a
getVal Nothing = error "Nothing happened"

toDeBruijn :: (Eq a) => Lambda a -> [Lambda a] -> [Lambda a] -> DeBruijn
toDeBruijn (Var v) l fr
  | Var v `elem` l = VarDB (getVal $ elemIndex (Var v) l)
  | otherwise = VarDB (length l + getVal (elemIndex (Var v) fr))
toDeBruijn (Abs s lamb) l fr = AbsDB $ toDeBruijn lamb (Var s : l) fr
toDeBruijn (App (Var v1) (Var v2)) l fr
  | Var v1 `notElem` l && Var v2 `notElem` l = AppDB (toDeBruijn (Var v1) l (fr ++ [Var v1] ++ [Var v2])) (toDeBruijn (Var v2) l (fr ++ [Var v1] ++ [Var v2]))
toDeBruijn (App lamb1 (Var v)) l fr
  | Var v `elem` l = AppDB (toDeBruijn lamb1 l fr) (toDeBruijn (Var v) l fr)
  | Var v `notElem` l = AppDB (toDeBruijn lamb1 l (fr ++ [Var v])) (toDeBruijn (Var v) l (fr ++ [Var v]))
toDeBruijn (App (Var v) lamb2) l fr
  | Var v `elem` l = AppDB (toDeBruijn (Var v) l fr) (toDeBruijn lamb2 l fr)
  | Var v `notElem` l = AppDB (toDeBruijn (Var v) l (fr ++ [Var v])) (toDeBruijn lamb2 l (fr ++ [Var v]))
toDeBruijn (App lamb1 lamb2) l fr = AppDB (toDeBruijn lamb1 l fr) (toDeBruijn lamb2 l fr)

-- Преобразовать деБрауновские лямбда-термы в обычные.

fromDeBruijn :: DeBruijn -> [a] -> [a] -> Lambda a
fromDeBruijn (VarDB v) _ currUse = Var $ currUse !! v
fromDeBruijn (AbsDB lamb) (x : varNames) currUse = Abs x $ fromDeBruijn lamb varNames (x : currUse)
fromDeBruijn (AppDB lamb1 lamb2) vN cU = App (fromDeBruijn lamb1 vN cU) (fromDeBruijn lamb2 vN cU)
fromDeBruijn _ _ _ = undefined

-- test  λx.λy.λs.λz.x s (y s z)  = Abs "x" (Abs "y" (Abs "s" (Abs "z"  (App (App (Var "x") (Var "s")) (App (App (Var "y") (Var "s")) (Var "z"))))))
