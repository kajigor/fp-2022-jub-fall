{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE BangPatterns #-}

module Lambda where
import Data.Map.Strict as Map
import qualified Control.Arrow as Data.Bifunctor
import Data.Set as Set

-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda a = Var a
              | App (Lambda a) (Lambda a)
              | Abs a (Lambda a)
              deriving (Eq)

instance Functor Lambda where
  fmap :: (a -> b) -> Lambda a -> Lambda b
  fmap func (Var a) = Var $ func a
  fmap func (App a b) = App (fmap func a) (fmap func b)
  fmap func (Abs a b) = Abs (func a) (fmap func b)

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

showImplicit :: Lambda [Char] -> [Char]
showImplicit (Var x) = x
showImplicit (App x y) = "(" ++ show (App x y) ++ ")"
showImplicit (Abs lx x) = "(" ++ show (Abs lx x) ++ ")"

-- Красивая печать без лишних скобок.
instance {-# OVERLAPS #-} Show (Lambda String) where
  show (Var x) = x
  show (App x@(Abs _ _) y) = showImplicit x ++ " " ++ showImplicit y
  show (App x y) = show x ++ " " ++ showImplicit y
  show (Abs lx x) = "λ" ++ lx ++ "." ++ show x

-- Красивая печать без лишних скобок.
instance Show a => Show (Lambda a) where
  show l = show (fmap show l)

alphaEqHelper :: Ord a => Map a a -> Lambda a -> Lambda a -> Bool
alphaEqHelper dict (Var x) (Var y) = case Map.lookup x dict of
  Just z -> y == z
  Nothing -> x == y
alphaEqHelper dict (App x y) (App t w) = alphaEqHelper dict x t && alphaEqHelper dict y w
alphaEqHelper dict (Abs x y) (Abs t w) = alphaEqHelper (Map.insert x t dict) y w
alphaEqHelper _ _ _ = False

-- Проверка термов на альфа-эквивалентность.
alphaEq :: Ord a => Lambda a -> Lambda a -> Bool
alphaEq = alphaEqHelper Map.empty


listMax :: Ord a => [a] -> Maybe a
listMax [] = Nothing
listMax (x:xs) = case listMax xs of
  Just y -> Just $ max x y
  Nothing -> Just x

class (Ord a, Show a, Eq a) => Listable a where
  vals :: [a]

instance Listable Int where
  vals = [1..]

letters :: [Char]
letters = ['a'..'z']

allWords :: [String]
allWords = concatMap allWordsLength [1..]
            where allWordsLength 0 = [""]
                  allWordsLength num = concatMap cc (allWordsLength (num - 1))
                  cc suff = Prelude.map (: suff) letters

instance Listable String where
  vals = allWords

getFresh :: (Listable a) => Set a -> a
getFresh = getFreshIter vals
                where getFreshIter (cur:ne) taken | cur `Set.notMember` taken = cur
                                                  | otherwise = getFreshIter ne taken
                      getFreshIter _ _ = undefined

putFresh :: (Ord k, Listable v) => Map k v -> k -> (v, Map k v)
putFresh dict x = (z, Map.insert x z dict)
                   where z = getFresh $ Set.fromList (Map.elems dict)

getOrPutFresh :: (Ord k, Listable v)  => Map k v -> k -> (v, Map k v)
getOrPutFresh dict x = case Map.lookup x dict of
  Just w -> (w, dict)
  Nothing -> putFresh dict x

-- Выберите подходящий тип для подстановок.
data Subst a = Subst (a, Lambda a)

fu :: Ord a => Lambda a -> Set a
fu (Var x) = Set.singleton x
fu (App x y) = fu x `Set.union` fu y
fu (Abs lx e) = lx `Set.delete` fu e

allVars :: Ord a => Lambda a -> Set a
allVars (Var x) = Set.singleton x
allVars (App x y) = fu x `Set.union` fu y
allVars (Abs lx e) = lx `Set.insert` fu e

-- Capture-avoiding substitution.
cas :: Listable a => Lambda a -> Subst a -> Lambda a
cas (Var y) (Subst (x, m)) | x == y = m
                           | otherwise = Var y
cas (App x y) sub = App (cas x sub) (cas y sub)
cas lam@(Abs y e) sub@(Subst (x, m)) | y == x = lam
                                     | y `Set.notMember` fu m = Abs y $ cas e sub
                                     | otherwise = Abs z $ cas (cas e $ Subst (y, Var z)) sub
                                        where z = getFresh $ x `Set.insert` (y `Set.insert` allVars e `Set.union` allVars m)

-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: Listable a => Strategy -> Lambda a -> Lambda a
eval CallByName e@(Var _) = e
eval CallByName e@(Abs _ _) = e
eval CallByName (App e1 e2) = case e1' of
  Abs lx e -> eval CallByName $ cas e $ Subst (lx, e2)
  _ -> App e1' e2
  where !e1' = eval CallByName e1

eval NormalOrder e@(Var _) = e
eval NormalOrder (Abs lx e) = Abs lx $ eval NormalOrder e
eval NormalOrder (App e1 e2) = case e1' of
  Abs lx e -> eval NormalOrder $ cas e $ Subst (lx, e2)
  _ -> App e1'' e2'
    where
      !e1'' = eval NormalOrder e1
      !e2' = eval NormalOrder e2
  where !e1' = eval CallByName e1

eval CallByValue e@(Var _) = e
eval CallByValue e@(Abs _ _) = e
eval CallByValue (App e1 e2) = case e1' of
  Abs lx e -> eval CallByValue $ cas e $ Subst (lx, e2')
  _ -> App e1' e2'
  where !e1' = eval CallByValue e1
        !e2' = eval CallByValue e2

eval ApplicativeOrder e@(Var _) = e
eval ApplicativeOrder (Abs lx e) = Abs lx $ eval ApplicativeOrder e
eval ApplicativeOrder (App e1 e2) = case e1' of
  Abs lx e -> eval ApplicativeOrder $ cas e $ Subst (lx, e2')
  _ -> App e1' e2'
  where !e1' = eval ApplicativeOrder e1
        !e2' = eval ApplicativeOrder e2

-- ДеБрауновское представление лямбда-термов
data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn

showImplicitDB :: DeBruijn -> String
showImplicitDB (VarDB x) = show x
showImplicitDB (AppDB x y) = "(" ++ show (AppDB x y) ++ ")"
showImplicitDB (AbsDB x) = "(" ++ show (AbsDB x) ++ ")"

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show (VarDB x) = show x
  show (AppDB x@(AbsDB _) y) = showImplicitDB x ++ " " ++ showImplicitDB y
  show (AppDB x y) = show x ++ " " ++ showImplicitDB y
  show (AbsDB x) = "λ " ++ show x

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

toDeBruijnHelper :: Ord a => Map a Int -> Lambda a -> (DeBruijn, Map a Int)
toDeBruijnHelper dict (Var x) = Data.Bifunctor.first VarDB (getOrPutFresh dict x)
toDeBruijnHelper dict (App x y) = Data.Bifunctor.first (AppDB (fst e1)) (toDeBruijnHelper (snd e1) y)
                                   where e1 = toDeBruijnHelper dict x
toDeBruijnHelper dict (Abs lx x) = Data.Bifunctor.first AbsDB (toDeBruijnHelper (snd $ putFresh dict lx) x)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Ord a => Lambda a -> DeBruijn
toDeBruijn x = fst $ toDeBruijnHelper Map.empty x

putFreshAuto :: Listable v => Map Int v -> (v, Map Int v)
putFreshAuto dict = putFresh dict $ case Map.lookupMax dict of
  Just (key, _) -> key + 1
  Nothing -> 1

fromDeBruijnHelper :: Listable a => Map Int a -> DeBruijn -> (Lambda a, Map Int a)
fromDeBruijnHelper dict (VarDB x) = Data.Bifunctor.first Var (getOrPutFresh dict x)
fromDeBruijnHelper dict (AppDB x y) = Data.Bifunctor.first (App (fst $ fromDeBruijnHelper dict x)) (fromDeBruijnHelper (snd $ fromDeBruijnHelper dict x) y)
fromDeBruijnHelper dict (AbsDB x) = Data.Bifunctor.first (Abs (fst $ putFreshAuto dict)) (fromDeBruijnHelper (snd $ putFreshAuto dict) x)

-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: Listable a => DeBruijn -> Lambda a
fromDeBruijn x = fst $ fromDeBruijnHelper Map.empty x
