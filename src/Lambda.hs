{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Lambda where

import Data.List (elemIndex)
import Data.Maybe
import GHC.Float (stgWord32ToFloat)
import System.Console.GetOpt (getOpt)

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
  show (App (Abs a l) lamb2) = "(" ++ show (Abs a l) ++ ") " ++ show lamb2
  show (App lamb1 (App b l)) = show lamb1 ++ "(" ++ show (App b l) ++ ")"
  show (App (App a l) lamb2) = show (App a l) ++ " " ++ show lamb2
  show (App lamb1 lamb2) = show lamb1 ++ " " ++ show lamb2

instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
  show (Var a) = show a
  show (Abs a lamb) = "λ" ++ show a ++ "." ++ show lamb
  show (App (Abs a l) lamb2) = "(" ++ show (Abs a l) ++ ") " ++ show lamb2
  show (App lamb1 (App b l)) = show lamb1 ++ "(" ++ show (App b l) ++ ")"
  show (App (App a l) lamb2) = show (App a l) ++ " " ++ show lamb2
  show (App lamb1 lamb2) = show lamb1 ++ " " ++ show lamb2

-- Выберите подходящий тип для подстановок.
--data Subst a = S Var a | B Lambda a
data SubVar a = SubVar a

data Subst a = Sub (SubVar a) (Lambda a)

-- Проверка термов на альфа-эквивалентность.
alphaEq :: Eq a => Lambda a -> Lambda a -> Bool
alphaEq lamb1 lamb2 = toDeBruijn lamb1 == toDeBruijn lamb2

boundedVar :: Lambda a -> [a] -> [a]
boundedVar (Var a) bounded = bounded
boundedVar (Abs a lamb) bounded = a : bounded ++ boundedVar lamb []
boundedVar (App lamb1 lamb2) bounded = bounded ++ boundedVar lamb1 [] ++ boundedVar lamb2 []

--Capture-avoiding substitution.
cas :: (Eq a) => Lambda a -> Subst a -> Lambda a
cas (Var a) (Sub (SubVar b) subLamb)
  | Var a == Var b = subLamb
  | otherwise = Var a
cas (App lamb1 lamb2) (Sub (SubVar b) subLamb) = App (cas lamb1 (Sub (SubVar b) subLamb)) (cas lamb2 (Sub (SubVar b) subLamb))
cas (Abs a lamb) (Sub (SubVar b) subLamb)
  | b == a = lamb
  | b /= a && a `elem` boundedVar subLamb [] = cas lamb (Sub (SubVar b) subLamb)
  | otherwise = undefined

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
toDeBruijn :: (Eq a) => Lambda a -> DeBruijn
toDeBruijn x = go x [] []
  where
    go :: (Eq a) => Lambda a -> [Lambda a] -> [Lambda a] -> DeBruijn
    go (Var v) l fr
      | Var v `elem` l = VarDB (fromJust $ elemIndex (Var v) l) -- unfortunately, I don't know how to fetch the index without fromJust
      | otherwise = VarDB (length l + fromJust (elemIndex (Var v) fr))
    go (Abs s lamb) l fr = AbsDB $ go lamb (Var s : l) fr
    go (App (Var v1) (Var v2)) l fr
      | Var v1 `notElem` l && Var v2 `notElem` l = AppDB (go (Var v1) l (fr ++ [Var v1] ++ [Var v2])) (go (Var v2) l (fr ++ [Var v1] ++ [Var v2]))
      | Var v1 `elem` l && Var v2 `notElem` l = AppDB (go (Var v1) l fr) (go (Var v2) l (fr ++ [Var v1] ++ [Var v2]))
      | Var v1 `notElem` l && Var v2 `elem` l = AppDB (go (Var v1) l (fr ++ [Var v1] ++ [Var v2])) (go (Var v2) l fr)
      | Var v1 `elem` l && Var v2 `elem` l = AppDB (go (Var v1) l fr) (go (Var v2) l fr)
    go (App lamb1 (Var v)) l fr
      | Var v `elem` l = AppDB (go lamb1 l fr) (go (Var v) l fr)
      | Var v `notElem` l = AppDB (go lamb1 l (fr ++ [Var v])) (go (Var v) l (fr ++ [Var v]))
    go (App (Var v) lamb2) l fr
      | Var v `elem` l = AppDB (go (Var v) l fr) (go lamb2 l fr)
      | Var v `notElem` l = AppDB (go (Var v) l (fr ++ [Var v])) (go lamb2 l (fr ++ [Var v]))
    go (App lamb1 lamb2) l fr = AppDB (go lamb1 l fr) (go lamb2 l fr)

-- Преобразовать деБрауновские лямбда-термы в обычные.

fromDeBruijn :: DeBruijn -> Lambda a
fromDeBruijn l = go l [] []
  where
    go (VarDB v) _ currUse = Var $ currUse !! v
    go (AbsDB lamb) (x : varNames) currUse = Abs x $ go lamb varNames (x : currUse)
    go (AppDB lamb1 lamb2) vN cU = App (go lamb1 vN cU) (go lamb2 vN cU)
    go _ _ _ = undefined
