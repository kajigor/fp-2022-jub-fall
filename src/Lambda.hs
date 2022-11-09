{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant where" #-}
module Lambda where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.RTS.Flags (getDebugFlags)
import Data.Maybe (isNothing)

-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda a = Var a
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
  show (App (Abs x l1) l2) = "(" ++ show (Abs x l1) ++ ") " ++ show l2
  show (App l1 (App l2 l3)) = show l1 ++ " (" ++ show (App l2 l3) ++ ")"
  show (App l r) = show l ++ " " ++ show r
  show (Abs l r) = "\\" ++ l ++ "." ++ show r

-- data Lambda a = Var a
--               | App (Lambda a) (Lambda a)
--               | Abs a (Lambda a)

instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
  show (Var x) = show x
  show (App (Abs x l1) l2) = "(" ++ show (Abs x l1) ++ ") " ++ show l2
  show (App l1 (App l2 l3)) = show l1 ++ " (" ++ show (App l2 l3) ++ ")"
  show (App l r) = show l ++ " " ++ show r
  show (Abs l r) = "\\" ++ show l ++ "." ++ show r

-- Выберите подходящий тип для подстановок.
data Subst a = Subst a (Lambda a) (Map.Map a a) -- переменная, заменитель, (map, которые рашает, на что заменять свободные переменные земенители при необходимости)
                                                -- значения в Map подразумеваются отличными от всех переменных в лямбде, в которой заменяем
-- Проверка термов на альфа-эквивалентность.
alphaEq :: Ord a => Ord b => Lambda a -> Lambda b -> Bool -- Ord по причинам описанным в toDeBruijn
alphaEq l r = toDeBruijn l == toDeBruijn r

-- Множество свободных переменных 
fv :: Ord a => Lambda a -> Set.Set a
fv (Var x) = Set.singleton x
fv (App l r) = Set.union (fv l) (fv r)
fv (Abs x l) = Set.delete x (fv l)

-- Capture-avoiding substitution.
cas :: Ord a => Lambda a -> Subst a -> Lambda a
cas (Var x) (Subst v s _) | x == v = s
                          | otherwise = Var x
cas (App l r) subst = App (cas l subst) (cas r subst)
cas (Abs x l) (Subst v s m) | x == v = Abs x l
                            | Set.notMember x (fv s) = Abs x (cas l (Subst v s m))
                            | otherwise = Abs (m Map.! x) (cas (cas l (Subst x (Var (m Map.! x)) Map.empty)) (Subst v s m))
                            -- можно передавать Map.empty, из-за предположения в 86 строчке
    

-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: Strategy -> Lambda a -> Lambda a
eval = undefined

-- ДеБрауновское представление лямбда-термов
data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn
              deriving (Eq)

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show (VarDB x) = show x
  show (AppDB (AbsDB l1) l2) = "(" ++ show (AbsDB l1) ++ ") " ++ show l2
  show (AppDB l1 (AppDB l2 l3)) = show l1 ++  " (" ++ show (AppDB l2 l3) ++ ")"
  show (AppDB l r) = show l ++ " " ++ show r
  show (AbsDB r) = "\\ " ++ show r

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Ord a => Lambda a -> DeBruijn -- выбор: либо (Ord a) и Map, либо (Hashable a) и HashMap 
                                            -- ну или хранить все в листе и делать все за квадрат
                                            -- (если без Map, то застрелиться можно)
toDeBruijn l = getDeBruijn l 0 Map.empty
  where
    getDeBruijn :: Ord a => Lambda a -> Int -> Map.Map a Int -> DeBruijn
    getDeBruijn (Var x) depth m | isNothing (Map.lookup x m) = VarDB (-1) -- Свободная переменная
                                | otherwise = VarDB (depth - (m Map.! x) + 1)
    getDeBruijn (Abs x rest) depth m = AbsDB (getDeBruijn rest (depth + 1) (Map.insert x (depth + 1) m))
    getDeBruijn (App l r) depth m = AppDB (getDeBruijn l depth m) (getDeBruijn r depth m)

-- add ≡ λm.λn.λf.λx.m f (n f x)
-- add = Abs "m" (Abs "n" (Abs "f" (Abs "x" (App (App (Var "m") (Var "f")) (App (App (Var "n") (Var "f")) (Var "x"))))))

-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: DeBruijn -> Lambda Int
fromDeBruijn d = fst $ fromDB d 0 0 Map.empty
  where
    fromDB :: DeBruijn -> Int -> Int -> Map.Map Int Int -> (Lambda Int, Int)
    fromDB (VarDB x) depth _ m | isNothing (Map.lookup x m) = (Var (-1), 0) -- Свободная переменная
                               | otherwise = (Var (m Map.! (depth - x + 1)), 0)
    fromDB (AbsDB rest) depth lastID m = 
      let (l, cnt) = fromDB rest (depth + 1) (lastID + 1) (Map.insert (depth + 1) (lastID + 1) m) in 
        (Abs (lastID + 1) l, cnt + 1)
    fromDB (AppDB l r) depth lastID m = 
      let (curL, cnt) = fromDB l depth lastID m in
      let (curR, res) = fromDB r depth (lastID + cnt) m in
      (App curL curR, res)
