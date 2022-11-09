{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Lambda where
import Data.Maybe
import Data.List
import Data.Tuple
import Data.Char
import qualified Data.Set as Set

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

class MyShow a where 
  _show :: a -> String

instance {-# OVERLAPPABLE #-} Show a => MyShow a where
  _show x = (show x)

instance {-# OVERLAPS #-} MyShow String where
  _show x = x

isAbs :: Lambda a -> Bool
isAbs (Abs x a) = True
isAbs _ = False

isNotVar :: Lambda a -> Bool
isNotVar (Var a) = False
isNotVar _ = True

showBrackets :: String -> Bool -> String
showBrackets str cond | cond == True = "(" ++ str ++ ")"
                      | otherwise = str

-- Красивая печать без лишних скобок.
instance MyShow a => Show (Lambda a) where
  show (Var a) = _show a
  show (Abs x term) = "λ" ++ (_show x) ++ "." ++ (show term)
  show (App a b) = (showBrackets (show a) (isAbs a)) ++ " " ++ (showBrackets (show b) (isNotVar b))


class Ord a => Freshable a where 
  getFresh :: Set.Set a -> a
  available :: [a]
  available = undefined
  getFresh st = (head $ filter notInLst available)
    where notInLst x = x `Set.notMember` st

instance Freshable String where
  available = [(gen x) | x <- [0..]]
    where gen x | x < 26 = [chr(x + (ord 'a'))]
                | otherwise = (gen (x `mod` 26)) ++ (gen (x `div` 26))

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

getVars :: Ord a => Lambda a -> Set.Set a
getVars (Var x) = Set.singleton x
getVars (App a b) = (getVars a) `Set.union` (getVars b)
getVars (Abs x a) = (Set.singleton x) `Set.union` (getVars a)

cas_with_closed :: Ord a => Freshable a => Lambda a -> Subst a -> Set.Set a -> Lambda a
cas_with_closed (Var a) (SubPair x subst) closed | (a == x) = subst
                                                 | otherwise = (Var a)
cas_with_closed (App a b) subst closed = (App (cas_with_closed a subst closed) (cas_with_closed b subst closed)) 
cas_with_closed (Abs x a) subst closed = (Abs y (cas_with_closed (cas_with_closed a (SubPair x (Var y)) (new_closed)) subst (new_closed)))
  where y = (getFresh closed)
        new_closed = (Set.singleton y) `Set.union` closed

-- Capture-avoiding substitution.
cas :: Ord a => Freshable a => Lambda a -> Subst a -> Lambda a
cas lambda (SubPair x subst) = cas_with_closed lambda (SubPair x subst) closed
  where closed = (getVars lambda) `Set.union` (getVars subst) 
        
-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: Ord a => Freshable a => Strategy -> Lambda a -> Lambda a
eval CallByValue lambda = evalCallByValue lambda 
eval CallByName lambda = evalCallByName lambda 
eval NormalOrder lambda = evalNormalOrder lambda 
eval ApplicativeOrder lambda = evalApplicativeOrder lambda 

evalCallByName :: Ord a => Freshable a => Lambda a -> Lambda a
evalCallByName (App a b) = helpfunction (evalCallByName a) b
  where helpfunction :: Ord a => Freshable a => Lambda a -> Lambda a -> Lambda a 
        helpfunction (Abs x first) second = (evalCallByName (cas_with_closed first (SubPair x second) ((getVars (Abs x first)) `Set.union` (getVars second))))
        helpfunction first second = (App first second)
evalCallByName lambda = lambda

evalNormalOrder :: Ord a => Freshable a =>Lambda a -> Lambda a
evalNormalOrder (Var x) = (Var x)
evalNormalOrder (Abs x a) = (Abs x (evalNormalOrder a))
evalNormalOrder (App a b) = helpfunction (evalCallByName a) b
  where helpfunction :: Ord a => Freshable a => Lambda a -> Lambda a -> Lambda a
        helpfunction (Abs x first) second = (evalNormalOrder (cas_with_closed first (SubPair x second) ((getVars (Abs x first)) `Set.union` (getVars second))))
        helpfunction first second = (App (evalNormalOrder first) (evalNormalOrder second))

evalCallByValue :: Ord a => Freshable a => Lambda a -> Lambda a
evalCallByValue (App a b) = helpfunction (evalCallByValue a) (evalCallByValue b)
  where helpfunction :: Ord a => Freshable a => Lambda a -> Lambda a -> Lambda a
        helpfunction (Abs x first) second = (evalCallByValue (cas_with_closed first (SubPair x second) ((getVars (Abs x first)) `Set.union` (getVars second))))
        helpfunction first second = (App first second)  
evalCallByValue lambda = lambda

evalApplicativeOrder :: Ord a => Freshable a => Lambda a -> Lambda a
evalApplicativeOrder (App a b) = helpfunction (evalApplicativeOrder a) (evalApplicativeOrder b)
  where helpfunction :: Ord a => Freshable a => Lambda a -> Lambda a -> Lambda a
        helpfunction (Abs x first) second = (evalApplicativeOrder (cas_with_closed first (SubPair x second) ((getVars (Abs x first)) `Set.union` (getVars second))))
        helpfunction first second = (App first second)
evalApplicativeOrder (Var x) = (Var x)
evalApplicativeOrder (Abs x a) = (Abs x (evalApplicativeOrder a))

-- ДеБрауновское представление лямбда-термов
data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn
              deriving Eq

isAbsDB :: DeBruijn -> Bool
isAbsDB (AbsDB a) = True
isAbsDB _ = False

isNotVarDB :: DeBruijn -> Bool
isNotVarDB (VarDB a) = False
isNotVarDB _ = True

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show (VarDB a) = _show a
  show (AbsDB term) = "λ" ++ "." ++ (show term)
  show (AppDB a b) = (showBrackets (show a) (isAbsDB a)) ++ " " ++ (showBrackets (show b) (isNotVarDB b))

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
        helpfunction (AbsDB g) lst h = Abs (getFresh (Set.fromList lst)) (helpfunction g (lst ++ [getFresh (Set.fromList lst)]) (succ h))
