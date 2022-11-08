{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Lambda where
import Data.Maybe
import Data.List
import Data.Tuple
import Data.Char

-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda a = Var a 
                | App (Lambda a) (Lambda a)
                | Abs a (Lambda a)
                deriving Eq


-- true ≡ λx.λy.x
true = Abs "x" (Abs "y" (Var "x"))

-- false ≡ λx.λy.y
false = Abs "x" (Abs "y" (Var "y"))

-- and ≡ λp.λq.p q p
and' = Abs "p" (Abs "q" (App (App (Var "p") (Var "q")) (Var "p")))

-- or ≡ λp.λq.p p q
or' = Abs "p" (Abs "q" (App (App (Var "p") (Var "p")) (Var "q")))

-- not ≡ λp.p FALSE TRUE
not' = Abs "p" (App (App (Var "p") false) true)

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
  show (Var a) = a
  show (Abs x term) = "λ" ++ x ++ "." ++ (show term)
  show (App (Var a) (Var b)) = a ++ " " ++ b
  show (App (Var a) b) = a ++ " (" ++ (show b) ++ ")"
  show (App (Abs x lambda) (Var c)) = "(" ++ (show (Abs x lambda)) ++ ") " ++ c   
  show (App (Abs x lambda) c) = "(" ++ (show (Abs x lambda)) ++ ") (" ++ (show c) ++ ")"
  show (App a (Var c)) = (show a) ++ " " ++ c   
  show (App a c) = (show a) ++ " (" ++ (show c) ++ ")"

 
instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
  show (Var a) = show a
  show (Abs x term) = "λ" ++ (show x) ++ "." ++ (show term)
  show (App (Var a) (Var b)) = (show a) ++ " " ++ (show b)
  show (App (Var a) b) = (show a) ++ " (" ++ (show b) ++ ")"
  show (App (Abs x lambda) (Var c)) = "(" ++ (show (Abs x lambda)) ++ ") " ++ (show c)   
  show (App (Abs x lambda) c) = "(" ++ (show (Abs x lambda)) ++ ") (" ++ (show c) ++ ")"
  show (App a (Var c)) = (show a) ++ " " ++ (show c)   
  show (App a c) = (show a) ++ " (" ++ (show c) ++ ")"

class Freshable a where 
  getFresh :: [a] -> a

available = Data.List.map (\x -> [x]) (['a'..'z'] ++ ['A'..'Z'])
instance Freshable String where
  getFresh :: [String] -> String
  getFresh lst = (fromMaybe "" (find (\x -> not (x `elem` lst)) available))

-- Выберите подходящий тип для подстановок.
data Subst a = SubPair a (Lambda a)

-- Проверка термов на альфа-эквивалентность.
alphaEq :: Eq a => Lambda a -> Lambda a -> Bool
alphaEq first second = helpfunction (toDeBruijn first) (toDeBruijn second)
  where helpfunction :: DeBruijn -> DeBruijn -> Bool
        helpfunction (VarDB x) (VarDB y) = (x == y)
        helpfunction (AppDB ax ay) (AppDB bx by) = ((helpfunction ax bx) && (helpfunction ay by))
        helpfunction (AbsDB av) (AbsDB bv) = (helpfunction av bv)
        helpfunction _ _ = False

getVars :: Lambda a -> [a]
getVars (Var x) = [x]
getVars (App a b) = (getVars a) ++ (getVars b)
getVars (Abs x a) = [x] ++ (getVars a)

cas_with_closed :: Eq a => Freshable a => Lambda a -> Subst a -> [a] -> Lambda a
cas_with_closed (Var a) (SubPair x subst) closed | (a == x) = subst
                                                 | otherwise = (Var a)
cas_with_closed (App a b) subst closed = (App (cas_with_closed a subst closed) (cas_with_closed b subst closed)) 
cas_with_closed (Abs x a) subst closed = (Abs y (cas_with_closed (cas_with_closed a (SubPair x (Var y)) (y : closed)) subst (y : closed)))
  where y = (getFresh closed)

-- Capture-avoiding substitution.
cas :: Eq a => Freshable a => Lambda a -> Subst a -> Lambda a
cas lambda (SubPair x subst) = cas_with_closed lambda (SubPair x subst) closed
  where closed = (getVars lambda) ++ (getVars subst) 
        
-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: Eq a => Freshable a => Strategy -> Lambda a -> Lambda a
eval CallByValue lambda = evalCallByValue lambda 
eval CallByName lambda = evalCallByName lambda 
eval NormalOrder lambda = evalNormalOrder lambda 
eval ApplicativeOrder lambda = evalApplicativeOrder lambda 

evalCallByName :: Eq a => Freshable a => Lambda a -> Lambda a
evalCallByName (App a b) = helpfunction (evalCallByName a) b
  where helpfunction :: Eq a => Freshable a => Lambda a -> Lambda a -> Lambda a 
        helpfunction (Abs x first) second = (evalCallByName (cas_with_closed first (SubPair x second) ((getVars (Abs x first)) ++ (getVars second))))
        helpfunction first second = (App first second)
evalCallByName lambda = lambda

evalNormalOrder :: Eq a => Freshable a =>Lambda a -> Lambda a
evalNormalOrder (Var x) = (Var x)
evalNormalOrder (Abs x a) = (Abs x (evalNormalOrder a))
evalNormalOrder (App a b) = helpfunction (evalCallByName a) b
  where helpfunction :: Eq a => Freshable a => Lambda a -> Lambda a -> Lambda a
        helpfunction (Abs x first) second = (evalNormalOrder (cas_with_closed first (SubPair x second) ((getVars (Abs x first)) ++ (getVars second))))
        helpfunction first second = (App (evalNormalOrder first) (evalNormalOrder second))

evalCallByValue :: Eq a => Freshable a => Lambda a -> Lambda a
evalCallByValue (App a b) = helpfunction (evalCallByValue a) (evalCallByValue b)
  where helpfunction :: Eq a => Freshable a => Lambda a -> Lambda a -> Lambda a
        helpfunction (Abs x first) second = (evalCallByValue (cas_with_closed first (SubPair x second) ((getVars (Abs x first)) ++ (getVars second))))
        helpfunction first second = (App first second)  
evalCallByValue lambda = lambda

evalApplicativeOrder :: Eq a => Freshable a => Lambda a -> Lambda a
evalApplicativeOrder (App a b) = helpfunction (evalApplicativeOrder a) (evalApplicativeOrder b)
  where helpfunction :: Eq a => Freshable a => Lambda a -> Lambda a -> Lambda a
        helpfunction (Abs x first) second = (evalApplicativeOrder (cas_with_closed first (SubPair x second) ((getVars (Abs x first)) ++ (getVars second))))
        helpfunction first second = (App first second)
evalApplicativeOrder (Var x) = (Var x)
evalApplicativeOrder (Abs x a) = (Abs x (evalApplicativeOrder a))

-- ДеБрауновское представление лямбда-термов
data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn
              deriving Eq

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show (VarDB a) = show a
  show (AbsDB term) = "λ" ++ "." ++ (show term)
  show (AppDB (VarDB a) (VarDB b)) = (show a) ++ " " ++ (show b)
  show (AppDB (VarDB a) b) = (show a) ++ " (" ++ (show b) ++ ")"
  show (AppDB (AbsDB lambda) (VarDB c)) = "(" ++ (show (AbsDB lambda)) ++ ") " ++ (show c)   
  show (AppDB (AbsDB lambda) c) = "(" ++ (show (AbsDB lambda)) ++ ") (" ++ (show c) ++ ")"
  show (AppDB a (VarDB c)) = (show a) ++ " " ++ (show c)  
  show (AppDB a c) = (show a) ++ " (" ++ (show c) ++ ")"

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Eq a => Lambda a -> DeBruijn
toDeBruijn term = helpfunction term [] 0
  where helpfunction :: Eq a => Lambda a -> [(a, Int)] -> Int -> DeBruijn
        helpfunction (Var x) lst h = VarDB (subtract (snd (fromMaybe (x, -10) (find (\p -> (fst p) == x) lst))) h)
        helpfunction (App f g) lst h  = AppDB (helpfunction f lst h) (helpfunction g lst h)
        helpfunction (Abs x g) lst h = AbsDB (helpfunction g ((x, h) : lst) (succ h))

-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: DeBruijn -> Lambda String
fromDeBruijn term = helpfunction term [] 0
  where helpfunction :: DeBruijn -> [String] -> Int -> Lambda String
        helpfunction (VarDB x) lst h = Var (lst!!(h - x))
        helpfunction (AppDB f g) lst h = App (helpfunction f lst h) (helpfunction g lst h)
        helpfunction (AbsDB g) lst h = Abs (getFresh lst) (helpfunction g (lst ++ [getFresh lst]) (succ h))
