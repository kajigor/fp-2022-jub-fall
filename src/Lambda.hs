{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lambda where

-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda a = Var a
              | App (Lambda a) (Lambda a)
              | Abs a (Lambda a)

data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn deriving Eq

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
instance {-# OVERLAPPABLE #-} Show a => MyShow a where
  showM = show

class MyShow a where
  showM :: a -> String

instance {-# OVERLAPS #-} MyShow String where
  showM a = a

instance MyShow a => Show (Lambda a) where
  show l = case l of
    Var s -> showM s
    Abs x y -> "\\" ++ (showM x) ++ "." ++ (showM y)  -- λ does not work on linux
    App (Abs x y) end -> "(" ++ (showM(Abs x y)) ++ ") " ++ (showM end)
    App beg (App x y) -> (showM beg) ++ "(" ++ (showM(App x y)) ++ ") "
    App x y -> showM x ++ " " ++ showM y


-- Выберите подходящий тип для подстановок.
data Subst a

-- Проверка термов на альфа-эквивалентность.
alphaEq :: (Eq a, Eq b) => Lambda a -> Lambda b -> Bool
alphaEq a b = (toDeBruijn a) == (toDeBruijn b)

-- Capture-avoiding substitution.
cas :: Lambda a -> Subst a -> Lambda a
cas = undefined

-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: Strategy -> Lambda a -> Lambda a
eval = undefined

-- ДеБрауновское представление лямбда-термов

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show (VarDB x) = showM x
  show (AbsDB x) = "\\" ++ "." ++ (showM x)
  show (AppDB (AbsDB x) end) = "(" ++ (showM(AbsDB x)) ++ ") " ++ (showM end)
  show (AppDB beg (AppDB x y)) = (showM beg) ++ "(" ++ (showM(AppDB x y)) ++ ") "
  show (AppDB x y) = showM x ++ " " ++ showM y

-- show (toDeBruijn Lambda.true)
-- show (toDeBruijn  (Abs "x" (Abs "y" (App (Var "x") (Var "y")))))
-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)
isMember :: Eq a => a -> [a] -> Int -> Int
isMember e [] d = -1
isMember e (x:xs) d
    | e == x = d
    | otherwise = isMember e xs (d + 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Eq a => Lambda a -> DeBruijn
toDeBruijn a = step a []
    where
        step :: Eq a => Lambda a -> [a]-> DeBruijn
        step (Var x) arr
            | ((isMember x arr 0) /= -1) = VarDB (isMember x arr 0)
            | otherwise = VarDB 239
        step (Abs x l) arr = AbsDB (step l (arr ++ [x]))
        step (App l1 l2) arr = AppDB (step l1 arr) (step l2 arr)


-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: DeBruijn -> Lambda a
fromDeBruijn = undefined