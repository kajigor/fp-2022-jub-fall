{-# LANGUAGE FlexibleInstances #-}
module Lambda where

import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Set as Set

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
mult = Abs "m" (Abs "n" (Abs "f" (App (Var "m") (App (Var "n") (Var "f")))))

-- mult' ≡ λm.λn.m (add n) 0
mult' = Abs "m" (Abs "n" (App (App (Var "m") (App add (Var "n"))) zero))

-- Красивая печать без лишних скобок.
instance {-# OVERLAPS #-} Show (Lambda String) where
  show expr = case expr of
    Var a -> a
    App a b -> addBrackets (show a) (isAbs a) Data.List.++ " " Data.List.++ addBrackets (show b) (isNotVar b)
    Abs a b -> "\\" Data.List.++ a Data.List.++ "." Data.List.++ show b

instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
  show expr = case expr of
    Var a -> show a
    App a b -> addBrackets (show a) (isAbs a) Data.List.++ " " Data.List.++ addBrackets (show b) (isNotVar b)
    Abs a b -> "\\" Data.List.++ show a Data.List.++ "." Data.List.++ show b

addBrackets :: String -> Bool -> String
addBrackets a f = if f then "(" Data.List.++ a Data.List.++ ")" else a

isAbs :: Lambda a -> Bool
isAbs (Abs _ _) = True
isAbs _ = False

isNotVar :: Lambda a -> Bool
isNotVar (Var _) = False
isNotVar _ = True

-- Выберите подходящий тип для подстановок.
data Subst a = Subst a (Lambda a)

-- Проверка термов на альфа-эквивалентность.
alphaEq :: Eq a => Lambda a -> Lambda a -> Bool
alphaEq a b = show (toDeBruijn a) == show (toDeBruijn b)

-- Capture-avoiding substitution.
cas :: Unique a => Lambda a -> Subst a -> Lambda a
cas (Var x) (Subst y a)
  | x == y = a
  | otherwise = Var x
cas (Abs x a) (Subst y b)
  | x == y = Abs x a
  | Set.member x (variables b) = Abs z (cas (cas a $ Subst x (Var z)) $ Subst y b)
  | otherwise = Abs x (cas a $ Subst y b)
  where z = make_unique (Set.insert y $ Set.union (variables a) (variables b))
cas (App x y) a = App (cas x a) (cas y a)

variables :: Ord a => Lambda a -> Set.Set a
variables (Var x) = Set.singleton x
variables (Abs x y) = Set.insert x (variables y)
variables (App x y) = Set.union (variables x) (variables y)

-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: Strategy -> Lambda a -> Lambda a
eval = undefined

-- ДеБрауновское представление лямбда-термов
data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show expr = case expr of
    VarDB a -> show a
    AppDB a b -> addBrackets (show a) (isAbsDB a) Data.List.++ " " Data.List.++ addBrackets (show b) (isNotVarDB b)
    AbsDB a -> "\\ " Data.List.++ show a

isAbsDB :: DeBruijn -> Bool
isAbsDB (AbsDB _) = True
isAbsDB _ = False

isNotVarDB :: DeBruijn -> Bool
isNotVarDB (VarDB _) = False
isNotVarDB _ = True

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Eq a => Lambda a -> DeBruijn
toDeBruijn = convert []
  where
    convert a (Var x) = VarDB (fromMaybe (Data.List.length a) (Data.List.elemIndex x a) + 1)
    convert a (Abs x y) = AbsDB (convert (x : a) y)
    convert a (App x y) = AppDB (convert a x) (convert a y)

-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: Unique a => DeBruijn -> Lambda a
fromDeBruijn = convert []
  where
    convert a (VarDB x) = Var (newVar a x)
    convert a (AbsDB y) = Abs x (convert (x : a) y) where x = newVar a (Data.List.length a + 1)
    convert a (AppDB x y) = App (convert a x) (convert a y)
    newVar a x = if x <= Data.List.length a then a Data.List.!! (x - 1)
                else make_unique (Set.fromList a)

class Ord a => Unique a where
  possible :: [a]

  make_unique :: Set.Set a -> a
  make_unique used = fromJust $ Data.List.find (`Set.notMember` used) possible

instance Unique String where
  possible = [kth_str n | n <- [0..]]
    where
      kth_str n = if n < 26 then [chr (ord 'a' + n)]
                  else kth_str (rem n 26) Data.List.++ kth_str (div n 26)
