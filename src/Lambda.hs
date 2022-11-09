{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE GADTs #-}
-- {-# LANGUAGE InstanceSigs #-}
module Lambda where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Array as Arr

-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda a = Var a
              | App (Lambda a) (Lambda a)
              | Abs a (Lambda a)
              deriving(Eq)

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
  show (Abs x y) = "λ" ++ x ++ "." ++ show y


instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
  show (Var x) = show x
  show (App x@(Abs _ _) y@(App _ _)) = "(" ++ show x ++ ") (" ++ show y ++ ")"
  show (App x@(Abs _ _) y) = "(" ++ show x ++ ") " ++ show y
  show (App x y@(App _ _)) = show x ++ " (" ++ show y ++ ")"
  show (App x y) = show x ++ " " ++ show y
  show (Abs x y) = "\\" ++ show x ++ "." ++ show y

-- Выберите подходящий тип для подстановок.
-- data Subst a

-- Проверка термов на альфа-эквивалентность.
alphaEq :: Ord a => Lambda a -> Lambda a -> Bool
alphaEq l1 l2 = alphaEqInternal l1 l2 m_def where
  m_def = zip (Set.toList $ getArgs l1 `Set.union` getArgs l2) [-1,-2..]
  alphaEqInternal l1' l2' m = toDeBruijn l1' m == toDeBruijn l2' m

getArgs :: Ord a => Lambda a -> Set.Set a
getArgs (Var v) = Set.singleton v
getArgs (App l1 l2) = Set.union (getArgs l1) (getArgs l2)
getArgs (Abs v l) = Set.insert v (getArgs l)


-- Capture-avoiding substitution.
-- cas :: Lambda a -> Subst a -> Lambda a
-- cas = undefined

-- Возможные стратегии редукции (о них расскажут 7 ноября).
-- data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
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


-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Ord a => Lambda a -> [(a, Int)] -> DeBruijn
toDeBruijn l l_def = internal (Map.empty, l) where
  m_def = Map.fromList l_def
  internal (m, Var v) | elm == 0 = VarDB $ m_def Map.! v
                      | otherwise = VarDB elm
    where
      size = Map.size m
      elm = size - Map.findWithDefault size v m
  internal (m, App x y) = AppDB (internal (m, x)) (internal (m, y))
  internal (m, Abs x y) = AbsDB (internal (Map.insert x (Map.size m) m, y))


-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: DeBruijn -> [(Int, a)] -> [a] -> Lambda a
fromDeBruijn l l_def names = internal (0, l) where
  m_def = Map.fromList l_def
  arr_names = Arr.array (1, length names) $ zip [1..] names
  internal (n, VarDB v) = Var elm where
    elm = if v <= n then arr_names Arr.! (n - v + 1) else m_def Map.! v
  internal (n, AppDB l1 l2) = App (internal (n, l1)) (internal (n, l2))
  internal (n, AbsDB l') = Abs (arr_names Arr.! (n + 1)) (internal (n + 1, l'))
