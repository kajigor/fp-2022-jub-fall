{-# LANGUAGE FlexibleInstances #-}
module Lambda where
import Control.Concurrent (yield)
import Data.Set as Set
import Data.Bits (Bits(xor))
import Control.Arrow (ArrowChoice(right))


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
  show (Var name) = name
  show (Abs x y) = "λ" ++ show x ++ "." ++ show y
  show (App x y) = left ++ " " ++ right
    where 
      right = case y of
        (Var _) -> show y
        _ -> "(" ++ show y ++ ")"
      left = case x of 
        (Abs _ _) -> "(" ++ show y ++ ")"
        _ -> show x
 
              


instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
  show (Var name) =  show name
  show (Abs x y) = "λ" ++ show x ++ "." ++ show y
  show (App x y) = left ++ " " ++ right
    where 
      right = case y of
        (Var _) -> show y
        _ -> "(" ++ show y ++ ")"
      left = case x of 
        (Abs _ _) -> "(" ++ show y ++ ")"
        _ -> show x
  

-- Выберите подходящий тип для подстановок.
data Subst a = Subst a (Lambda a)

-- Проверка термов на альфа-эквивалентность.
alphaEq :: Eq a => Lambda a -> Lambda a -> Bool
alphaEq x y =  toDeBruijn x == toDeBruijn y


class VarGenerator x where
  getVar :: x -> Set.Set x -> x

instance VarGenerator String  where
  getVar name vars | Set.member name vars = getVar (name ++ "v") vars
                   | otherwise = name



-- Capture-avoiding substitution.
cas :: (Ord a, VarGenerator a) => Lambda a -> Subst a -> Lambda a
cas (Var x) (Subst y z) | x == y = z
                        | otherwise = Var x
cas (App x y) sub = App (cas x sub) (cas y sub)          
cas (Abs x y) (Subst z m) | x == z = Abs x y
                          | x == z = Abs x y
                          | Set.notMember x (freeVars m) = Abs x (cas y (Subst z m) )
                          | otherwise = Abs nextVar $ cas (cas y (Subst x (Var nextVar))) (Subst z m)
                            where 
                              nextVar = getVar x (Set.union (allVars y) (allVars m))

                              freeVars (Var a) = Set.singleton a
                              freeVars (App a b) = Set.union (freeVars a) (freeVars b)
                              freeVars (Abs a b) = Set.delete a $ (freeVars b)
                  
                              allVars (Var a) = Set.singleton a
                              allVars (App a b) = Set.union (allVars a) (allVars b)
                              allVars (Abs a b) = Set.insert a (allVars b) 


-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: (Ord a, VarGenerator a) => Strategy -> Lambda a -> Lambda a
eval _ (Var x) = Var x

eval NormalOrder (Abs x y) =  Abs x $ eval NormalOrder y
eval ApplicativeOrder (Abs x y) = Abs x $ eval ApplicativeOrder y
eval _ (Abs x y) = Abs x y

eval CallByValue (App x y) = case evx of 
  Abs a b -> eval CallByValue $ cas b (Subst a evy)
  _ -> App evx evy
  where 
    evx = eval CallByValue x
    evy = eval CallByValue y
eval CallByName (App x y) = case evx of
  Abs a b -> eval CallByName $ cas b  (Subst a y)
  _ -> App evx y
  where
    evx = eval CallByName x
eval NormalOrder (App x y) = case evx of
  Abs a b -> eval NormalOrder $ cas b  (Subst a b)
  _ -> App (eval NormalOrder evx) (eval NormalOrder y)
  where 
    evx = eval NormalOrder x
eval ApplicativeOrder (App x y) = case evx of
  Abs a b -> eval ApplicativeOrder $ cas b (Subst a evy)
  _ -> App evx evy
  where
    evx = eval ApplicativeOrder x
    evy = eval ApplicativeOrder y

-- ДеБрауновское представление лямбда-термов
data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn
              deriving (Eq)

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show (VarDB x) = show x
  show (AbsDB x) = "λ." ++ show x
  show (AppDB x y) = left ++ " "  ++ right
    where 
      left = case x of
        (AbsDB _) -> "(" ++ show x ++  ")"
        _ -> show x
      right = case y of
        (VarDB _) -> show y
        _ -> "(" ++ show y ++  ")"


-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Lambda a -> DeBruijn
toDeBruijn = undefined

-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: DeBruijn -> Lambda a
fromDeBruijn = undefined