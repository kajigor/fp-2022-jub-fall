{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE InstanceSigs #-}
module Lambda where

import qualified Data.Map as Map

-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda a = Var a
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
mult = Abs "m" (Abs "n" (App (Var "m") (App (Var "n") (Var "f"))))

-- mult' ≡ λm.λn.m (add n) 0
mult' = Abs "m" (Abs "n" (App (App (Var "m") (App add (Var "n"))) zero))

-- Красивая печать без лишних скобок.
instance {-# OVERLAPS #-} Show (Lambda String) where
  show (Var x) = x
  show (App x@(Abs _ _) y@(App _ _)) = "(" ++ show x ++ ") (" ++ show y ++ ")"
  show (App x@(Abs _ _) y) = "(" ++ show x ++ ") " ++ show y
  show (App x y@(App _ _)) = show x ++ " (" ++ show y ++ ")"
  show (App x y) = show x ++ " " ++ show y
  show (Abs x y) = "\\" ++ x ++ "." ++ show y


instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
  show (Var x) = show x
  show (App x@(Abs _ _) y@(App _ _)) = "(" ++ show x ++ ") (" ++ show y ++ ")"
  show (App x@(Abs _ _) y) = "(" ++ show x ++ ") " ++ show y
  show (App x y@(App _ _)) = show x ++ " (" ++ show y ++ ")"
  show (App x y) = show x ++ " " ++ show y
  show (Abs x y) = "\\" ++ show x ++ "." ++ show y

-- Выберите подходящий тип для подстановок.
data Subst a

-- -- Проверка термов на альфа-эквивалентность.
-- alphaEq :: Eq a => Lambda a -> Lambda a -> Bool
-- alphaEq = undefined

-- -- Capture-avoiding substitution.
-- cas :: Lambda a -> Subst a -> Lambda a
-- cas = undefined

-- -- Возможные стратегии редукции (о них расскажут 7 ноября).
-- data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- -- Интерпретатор лямбда термов, учитывающий стратегию.
-- eval :: Strategy -> Lambda a -> Lambda a
-- eval = undefined

-- ДеБрауновское представление лямбда-термов
data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn
              deriving(Eq)

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show (VarDB x) = show x
  show (AppDB x@(AbsDB _) y@(AppDB _ _)) = "(" ++ show x ++ ") (" ++ show y ++ ")"
  show (AppDB x@(AbsDB _) y) = "(" ++ show x ++ ") " ++ show y
  show (AppDB x y@(AppDB _ _)) = show x ++ " (" ++ show y ++ ")"
  show (AppDB x y) = show x ++ " " ++ show y
  show (AbsDB d) = "λ " ++ show d

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- -- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Ord a => Lambda a -> DeBruijn
toDeBruijn l = internal (Map.empty, l) where
  internal :: Ord a => (Map.Map a Int, Lambda a) -> DeBruijn
  internal (m, Var v) = let size = Map.size m in VarDB $ size - Map.findWithDefault size v m
  internal (m, App x y) = AppDB (internal (m, x)) (internal (m, y))
  internal (m, Abs x y) = AbsDB (internal (Map.insert x (Map.size m) m, y))


-- -- Преобразовать деБрауновские лямбда-термы в обычные.
-- fromDeBruijn :: DeBruijn -> Lambda a
-- fromDeBruijn = undefined