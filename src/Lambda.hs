{-# LANGUAGE FlexibleInstances #-}
module Lambda where

-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda a = Var a
              | App (Lambda a) (Lambda a)
              | Abs a (Lambda a)

-- true ≡ λx.λy.x
true :: Lambda [Char]
true = Abs "x" (Abs "y" (Var "x"))

-- false ≡ λx.λy.y
false :: Lambda [Char]
false = Abs "x" (Abs "y" (Var "y"))

-- and ≡ λp.λq.p q p
and :: Lambda [Char]
and = Abs "p" (Abs "q" (App (App (Var "p") (Var "q")) (Var "p")))

-- or ≡ λp.λq.p p q
or :: Lambda [Char]
or = Abs "p" (Abs "q" (App (App (Var "p") (Var "p")) (Var "q")))

-- not ≡ λp.p FALSE TRUE
not :: Lambda [Char]
not = Abs "p" (App (App (Var "p") false) true)

-- ifThenElse ≡ λp.λa.λb.p a b
ifThenElse :: Lambda [Char]
ifThenElse = Abs "p" (Abs "a" (Abs "b" (App (App (Var "p") (Var "a")) (Var "b"))))

-- zero ≡ λf.λx.x
zero :: Lambda [Char]
zero = Abs "f" (Abs "x" (Var "x"))

-- one ≡ λf.λx.f x
one :: Lambda [Char]
one = Abs "f" (Abs "x" (App (Var "f") (Var "x")))

-- two ≡ λf.λx.f (f x)
two :: Lambda [Char]
two = Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (Var "x"))))

-- three ≡ λf.λx.f (f (f x))
three :: Lambda [Char]
three =  Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (Var "x")))))

--  four ≡ λf.λx.f (f (f (f x)))
four :: Lambda [Char]
four =  Abs "f" (Abs "x" (App (Var "f") (App (Var "f") (App (Var "f") (App (Var "f") (Var "x"))))))

-- add ≡ λm.λn.λf.λx.m f (n f x)
add :: Lambda [Char]
add = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x"))))))

-- successor ≡ λn.λf.λx.f (n f x)
successor :: Lambda [Char]
successor = Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x")))))

-- add' ≡ λm.λn.m successor n
add' :: Lambda [Char]
add' = Abs "m" (Abs "n" (App (App (Var "m") successor) (Var "n")))

-- mult ≡ λm.λn.λf.m (n f)
mult :: Lambda [Char]
mult = Abs "m" (Abs "n" (App (Var "m") (App (Var "n") (Var "f"))))

-- mult' ≡ λm.λn.m (add n) 0
mult' :: Lambda [Char]
mult' = Abs "m" (Abs "n" (App (App (Var "m") (App add (Var "n"))) zero))

-- Красивая печать без лишних скобок.
instance {-# OVERLAPS #-} Show (Lambda String) where
  show (Var x) = x
  show (Abs x y) = "\\" ++ x ++ "." ++ show y
  show (App lambda1 lambda2) = lambda1_2 ++ " " ++ lambda2_2 where
    lambda1_2 = go lambda1 where
      go (Abs _ _) = "(" ++ show lambda1 ++ ")"
      go _ = show lambda1
    lambda2_2 = go lambda2 where
      go (Var _) = show lambda2
      go _ = "(" ++ show lambda2 ++ ")"

instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
  show (Var x) = show x
  show (Abs x y) = "\\" ++ show x ++ "." ++ show y
  show (App lambda1 lambda2) = lambda1_2 ++ " " ++ lambda2_2 where
    lambda1_2 = go lambda1 where
      go (Abs _ _) = "(" ++ show lambda1 ++ ")"
      go _ = show lambda1
    lambda2_2 = go lambda2 where
      go (Var _) = show lambda2
      go _ = "(" ++ show lambda2 ++ ")"

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

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show = undefined

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Lambda a -> DeBruijn
toDeBruijn = undefined

-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: DeBruijn -> Lambda a
fromDeBruijn = undefined
