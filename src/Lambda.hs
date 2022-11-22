{-# LANGUAGE FlexibleInstances #-}
module Lambda where

import Control.Monad.Error (Error(strMsg))

import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import qualified Data.Set as Set


-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda a = Var a
              | App (Lambda a) (Lambda a)
              | Abs a (Lambda a)
              deriving(Eq)

isVar::Lambda a -> Bool
isVar (Var _) = True
isVar _ = False

isApp::Lambda a -> Bool
isApp App{} = True
isApp _ = False

isAbs::Lambda a -> Bool
isAbs Abs{} = True
isAbs _ = False

wrapIf:: Bool -> String -> String
wrapIf True s = "(" ++ s ++ ")"
wrapIf False s = s

-- Красивая печать без лишних скобок.
instance {-# OVERLAPS #-} Show (Lambda String) where
  show (Var str) = str
  show (App x y) = wrapIf (isAbs x) (show x) ++ " " ++ wrapIf (isAbs y || isApp y) (show y)
  show (Abs x y) = "\\" ++ x ++ "." ++ show y

instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
  show = undefined

-- Проверка термов на альфа-эквивалентность.
alphaEq :: Eq a => Lambda a -> Lambda a -> Bool
alphaEq a b = toDeBruijn a == toDeBruijn b

allVars :: Ord a => Lambda a -> Set.Set a
allVars (Var x) = Set.singleton x
allVars (App x y) = allVars x `Set.union` allVars y
allVars (Abs x y) = Set.singleton x `Set.union` allVars y

boundVars :: Ord a => Lambda a -> Set.Set a
boundVars Var{} = Set.empty
boundVars (App x y) = boundVars x `Set.union` boundVars y
boundVars (Abs x y) = Set.singleton x `Set.union` boundVars y

freeVars :: Ord a => Lambda a -> Set.Set a
freeVars term = allVars term `Set.difference` boundVars term

class Ord a => NextFree a where
  nextFree :: a -> a

instance NextFree String where
  nextFree [] = "a"
  nextFree str@('z':_) = 'a':show (length str)
  nextFree (s:xs) = chr (ord s + 1):xs


-- Выберите подходящий тип для подстановок.
data Subst a = Subst a (Lambda a)

-- Capture-avoiding substitution.
cas :: NextFree a => Lambda a -> Subst a -> Lambda a
cas (Var x) (Subst var term) 
  | x == var = term
  | otherwise = Var x
cas (App x y) sub = App (cas x sub) (cas y sub)
cas orig@(Abs x y) sub@(Subst var term) 
  | x == var = orig
  | x `Set.notMember` freeVars term = Abs x (cas y sub)
  | otherwise = Abs z (cas updatdOrig (Subst var term))
  where 
    z = nextFree $ Set.findMax (allVars orig `Set.union` allVars term)
    updatdOrig = cas y (Subst x (Var z))


-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder deriving(Eq)

bRed :: NextFree a=> Lambda a -> Lambda a
bRed (App (Abs x y) z) = y `cas` Subst x z
bRed _ = undefined

evalApp :: NextFree a=> Strategy -> Lambda a -> Lambda a -> Lambda a
evalApp CallByName x y = do
  let x' = eval CallByName x
  if isAbs x'
    then eval CallByName (bRed (App x' y))
    else App x' y

evalApp NormalOrder x y = do
  let x' = eval CallByName x
  if isAbs x'
    then eval NormalOrder (bRed (App x' y))
    else do
      let x'' = eval NormalOrder x'
      let y' = eval NormalOrder y
      App x'' y'

evalApp CallByValue x y = do
  let x' = eval CallByValue x
  let y' = eval CallByValue y
  if isAbs x'
    then eval CallByValue (bRed (App x' y))
    else App x' y'

evalApp ApplicativeOrder x y = do
  let x' = eval ApplicativeOrder x
  let y' = eval ApplicativeOrder y
  if isAbs x'
    then eval ApplicativeOrder (bRed (App x' y))
    else App x' y'


-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: NextFree a=> Strategy -> Lambda a -> Lambda a
eval _ (Var x) = Var x
eval strat (Abs x y) 
  | strat == CallByName || strat == CallByValue = Abs x y 
  | otherwise = Abs x (eval strat y)
eval strat (App x y) = evalApp strat x y


-- ДеБрауновское представление лямбда-термов
data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn
              deriving(Eq)

isVarDB::DeBruijn -> Bool
isVarDB VarDB{} = True
isVarDB _ = False

isAppDB::DeBruijn -> Bool
isAppDB AppDB{} = True
isAppDB _ = False

isAbsDB::DeBruijn -> Bool
isAbsDB AbsDB{} = True
isAbsDB _ = False

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show (VarDB num) = show num
  show (AppDB x y) = wrapIf (isAbsDB x) (show x) ++ " " ++ wrapIf (isAbsDB y || isAppDB y) (show y)
  show (AbsDB x) = "\\ " ++ show x

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Eq a => Lambda a -> DeBruijn
toDeBruijn term =
  evalState (go [] term) []
  where
    go :: Eq a => [a] -> Lambda a-> State [a] DeBruijn
    go acc (Var x)
      | isJust $ elemIndex x acc = return $ VarDB $ fromJust (elemIndex x acc)
      | otherwise = do
        free <- get
        if isNothing $ elemIndex x free
          then do
            put (free ++ [x])
            return $ VarDB (length free + length acc)
          else
            return $ VarDB (fromJust (elemIndex x free) + length acc)
    go acc (App x y) = do
      x' <- go acc x
      y' <- go acc y
      return (AppDB x' y')
    go acc (Abs x y) = do
      x' <- go (x:acc) y
      return (AbsDB x')
    

-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: DeBruijn -> Lambda String
fromDeBruijn debruijn =
  evalState (go debruijn []) "a"
  where
    go:: DeBruijn -> [String] -> State String (Lambda String)
    go (VarDB x) xs | x >= length xs = do
        next <- get
        put (nextFree next)
        return (Var next)
                    | otherwise = return (Var (xs!!x))
    go (AppDB x y) xs = do
      x' <- go x xs
      y' <- go y xs
      return (App x' y')
    go (AbsDB x) xs = do
      next <- get
      put (nextFree next)
      x' <- go x (next:xs)
      return (Abs next x')


-- Lambdas
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


-- Lambdas DeBruijn
-- trueDB ≡ \\ \\ 1
trueDB = AbsDB (AbsDB (VarDB 1))

-- falseDB ≡ \\ \\ 0
falseDB = AbsDB (AbsDB (VarDB 0))

-- andDB ≡ \\ \\ 1 0 1
andDB = AbsDB (AbsDB (AppDB (AppDB (VarDB 1) (VarDB 0)) (VarDB 1)))

-- orDB ≡ \\ \\ 1 1 0
orDB = AbsDB (AbsDB (AppDB (AppDB (VarDB 1) (VarDB 1)) (VarDB 0)))

-- notDB ≡ \\ 0 (\\ \\ 0) (\\ \\ 1)
notDB = AbsDB (AppDB (AppDB (VarDB 0) falseDB) trueDB)

-- ifThenElseDB ≡ \\ \\ \\ 2 1 0
ifThenElseDB = AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 2) (VarDB 1)) (VarDB 0))))

-- zeroDB ≡ \\ \\ 0
zeroDB = AbsDB (AbsDB (VarDB 0))

-- oneDB ≡ \\ \\ 1 0
oneDB = AbsDB (AbsDB (AppDB (VarDB 1) (VarDB 0)))

-- threeDB ≡ \\ \\ 1 (1 (1 0))
threeDB =  AbsDB (AbsDB (AppDB (VarDB 1) (AppDB (VarDB 1) (AppDB (VarDB 1) (VarDB 0)))))

-- addDB ≡ \\ \\ \\ \\ 3 1 (2 1 0)
addDB = AbsDB (AbsDB (AbsDB (AbsDB (AppDB (AppDB (VarDB 3) (VarDB 1)) (AppDB (AppDB (VarDB 2) (VarDB 1)) (VarDB 0))))))

-- successorDB ≡ \\ \\ \\ 1 (2 1 0)
successorDB = AbsDB (AbsDB (AbsDB (AppDB (VarDB 1) (AppDB (AppDB (VarDB 2) (VarDB 1)) (VarDB 0)))))

-- multDB ≡ \\ \\ \\ 2 (1 0)
multDB = AbsDB (AbsDB (AbsDB (AppDB (VarDB 2) (AppDB (VarDB 1) (VarDB 0)))))
