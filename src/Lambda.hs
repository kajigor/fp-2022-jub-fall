{-# LANGUAGE FlexibleInstances #-}
module Lambda where

import Data.List as List
import Data.Set as Set
import Data.Map.Strict as Map

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

instance Functor Lambda where
  fmap f (App x y) = App (fmap f x) (fmap f y)
  fmap f (Abs x y) = Abs (f x) (fmap f y)
  fmap f (Var x) = Var $ f x

-- Красивая печать без лишних скобок.
instance {-# OVERLAPS #-} Show (Lambda String) where
  show (Var x) = x
  show (App x y) = left ++ " " ++ right
    where
      right = case y of 
        (Var _) -> show y
        _ -> wrap (show y)

      left = case x of
        (Abs _ _) -> wrap (show x)
        _ -> show x

      wrap :: String -> String
      wrap s = "(" ++ s ++ ")"
  show (Abs x y) = "\\" ++ x ++ "." ++ (show y)

instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
  show l = show (fmap show l)

-- Проверка термов на альфа-эквивалентность.
alphaEq :: Ord a => Lambda a -> Lambda a -> Bool
alphaEq l1 l2 = alphaEqWithBV l1 l2 Set.empty Map.empty
  where
    alphaEqWithBV :: Ord a => Lambda a -> Lambda a -> Set.Set a -> Map.Map a a -> Bool
    alphaEqWithBV (Var x) (Var y) bv1 bv2 = 
        let mapy = Map.lookup y bv2 in
        case Set.member x bv1 of
          True -> mapy == Just x
          False -> mapy == Nothing && y == x
    alphaEqWithBV (App a b) (App c d) bv1 bv2 = alphaEqWithBV a c bv1 bv2 && alphaEqWithBV b d bv1 bv2
    alphaEqWithBV (Abs x a) (Abs y b) bv1 bv2 = alphaEqWithBV a b (Set.insert x bv1) (Map.insert y x bv2)
    alphaEqWithBV _ _ _ _ = False



-- Выберите подходящий тип для подстановок.
data Subst a = Subst a (Lambda a)

class Indactive a where
  zero' :: a
  inc :: a -> a

instance Indactive Int where
  zero' = 0
  inc a = a + 1

instance Indactive String where
  zero' = ""
  inc a = a ++ "a"

fresh :: Ord a => Indactive a => Set.Set a -> a
fresh set = try set (inc zero')
  where
    try set i | Set.member i set = try set (inc i)
              | otherwise = i

-- Capture-avoiding substitution.
cas :: Ord a => Indactive a => Lambda a -> Subst a -> Lambda a
cas (Var x) (Subst y m) | x == y = m
                        | otherwise = Var x
cas (App l1 l2) subst = App (cas l1 subst) (cas l2 subst)
cas (Abs x l) subst@(Subst y m) | x == y = Abs x l
                                | (Set.notMember x $ freeVar m) = Abs x (cas l subst)
                                | otherwise = let w = fresh $ Set.union (allVars l) (allVars m) in
                                  Abs w $ cas (cas l $ Subst x (Var w)) subst
  where
    freeVar :: Ord a => Indactive a => Lambda a -> Set.Set a
    freeVar (Var x) = Set.singleton x
    freeVar (App l1 l2) = Set.union (freeVar l1) (freeVar l2)
    freeVar (Abs x l) = Set.delete x $ freeVar l

    allVars (Var x) = Set.singleton x
    allVars (App l1 l2) = Set.union (allVars l1) (allVars l2)
    allVars (Abs x l) = Set.insert x (allVars l)




-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: Ord a => Indactive a => Strategy -> Lambda a -> Lambda a
eval CallByName (App e1 e2) = 
  let e1' = eval CallByName e1 in
  case e1' of
    (Abs x e) -> eval CallByName $ cas e $ Subst x e2
    _ -> App e1' e2
eval CallByName x = x

eval NormalOrder (Abs x e) = Abs x $ eval NormalOrder e
eval NormalOrder (App e1 e2) = 
  let e1' = eval CallByName e1 in
  case e1' of
    (Abs x e) -> eval NormalOrder $ cas e $ Subst x e2
    _ -> let e1'' = eval NormalOrder e1' in 
      let e2' = eval NormalOrder e2 in 
      App e1'' e2'
eval NormalOrder x = x

eval CallByValue (App e1 e2) =
  let e1' = eval CallByValue e1 in
  let e2' = eval CallByValue e2 in 
  case e1' of
    (Abs x e) -> eval CallByValue $ cas e $ Subst x $! e2'
    _ -> App e1' e2'
eval CallByValue x = x

eval ApplicativeOrder (Abs x e) = Abs x $ eval ApplicativeOrder e
eval ApplicativeOrder (App e1 e2) =
  let e1' = eval ApplicativeOrder e1 in
  let e2' = eval ApplicativeOrder e2 in
  case e1' of
    (Abs x e) -> eval ApplicativeOrder $ cas e $ Subst x $! e2'
    _ -> App e1' e2'
eval ApplicativeOrder x = x


-- ДеБрауновское представление лямбда-термов
data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn
              deriving (Eq)

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show (VarDB i) = show i
  show (AbsDB l) = "\\." ++ (show l)
  show (AppDB l1 l2) = left ++ " " ++ right
    where
      right = case l2 of 
        (VarDB _) -> show l2
        _ -> "(" ++ (show l2) ++ ")"

      left = case l1 of
        (AbsDB _) -> "(" ++ (show l1) ++ ")"
        _ -> show l1

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Eq a => Lambda a -> Maybe DeBruijn
toDeBruijn l = go l []
  where
    go :: Eq a => Lambda a -> [a] -> Maybe DeBruijn
    go (Var x) list = do
      let i = List.elemIndex x list
      case i of
        Just pos -> Just $ VarDB pos
        Nothing -> Nothing -- free variable
    go (App l1 l2) list = do
      l1' <- go l1 list
      l2' <- go l2 list
      return $ AppDB l1' l2'
    go (Abs x l) list = do
      l' <- go l $ x : list
      return $ AbsDB l'


-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: Indactive a => DeBruijn -> Lambda a
fromDeBruijn l = go l [] zero'
  where 
    go :: Indactive a => DeBruijn -> [a] -> a -> Lambda a
    go (VarDB i) list last = Var $ list !! i
    go (AppDB l1 l2) list last = App (go l1 list last) (go l2 list last)
    go (AbsDB l) list last = let new_last = inc last in Abs new_last (go l (new_last : list) new_last)