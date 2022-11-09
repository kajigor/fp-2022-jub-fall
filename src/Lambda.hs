{-# LANGUAGE FlexibleInstances #-}
module Lambda where

import qualified Data.HashMap.Strict as Map
import Data.Hashable (Hashable)
import Data.Function (on)

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
  show (App a b) = show a ++ " " ++ showWithBraces b
    where
      foldIntoBraces :: String -> String
      foldIntoBraces a = "(" ++ a ++ ")"

      showWithBraces :: Lambda String -> String
      showWithBraces (Var a) = a
      showWithBraces a = foldIntoBraces $ show a
  show (Abs var a) = "λ" ++ var ++ "." ++ show a


instance {-# OVERLAPPABLE #-} Show a => Show (Lambda a) where
  show (Var name) = show name
  show (App a b) = show a ++ " " ++
    case b of -- не очень понимаю, ghc отказлся принимать showWithBraces здесь по причине overlapping'а show
      Var x -> show x
      x -> "(" ++ show x ++ ")"
  show (Abs var a) = "λ" ++ show var ++ "." ++ show a

-- Выберите подходящий тип для подстановок.
data Subst a = Subst {from :: a, to :: Lambda a}

-- Проверка термов на альфа-эквивалентность.
alphaEq :: (Eq a, Hashable a, Eq b, Hashable b) => Lambda a -> Lambda b -> Bool
alphaEq a b = toDeBruijn a == toDeBruijn b

freeVariables :: Eq a => Lambda a -> [a]
freeVariables (Var x) = [x]
freeVariables (App a b) = ((++) `on` freeVariables) a b
freeVariables (Abs x e) = filter (/=x) $ freeVariables e

allVariables ::  Eq a => Lambda a -> [a]
allVariables (Var x) = [x]
allVariables (App e1 e2) = ((++) `on` allVariables) e1 e2
allVariables (Abs x e) = x : allVariables e


class Freshable a where
  getNext :: a -> [a] -> a

instance Freshable String where
  getNext var variables | var `elem` variables = getNext (var ++ "'") variables
                        | otherwise = var



-- Capture-avoiding substitution.
cas :: (Eq a, Freshable a) => Lambda a -> Subst a -> Lambda a
cas (Var x) subst | x == from subst = to subst
                  | otherwise = Var x
cas (App a b) subst = App (cas a subst) (cas b subst)
cas (Abs x e) subst | x == from subst = Abs x e
                    | x `notElem` freeVariables (to subst) = Abs x (cas e subst)
                    | otherwise = Abs next $ cas (cas e Subst {from = x, to = Var next} ) subst
                      where
                        next = getNext x (allVariables e ++ allVariables (to subst))



-- Возможные стратегии редукции (о них расскажут 7 ноября).
data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder

-- Интерпретатор лямбда термов, учитывающий стратегию.
eval :: Eq a => Freshable a => Strategy -> Lambda a -> Lambda a
eval _ (Var x) = Var x

eval CallByValue (Abs x e) = Abs x e
eval CallByValue (App e1 e2) = let e1' = eval CallByValue e1 in
  let e2' = eval CallByValue e2 in
    case e1' of
      Abs x e -> eval CallByValue $ cas e Subst {from = x, to = e2'}
      _ -> App e1' e2'

eval CallByName (Abs x e) = Abs x e
eval CallByName (App e1 e2) = let evaluated = eval CallByName e1 in
  case evaluated of
    Abs x e1' -> eval CallByName $ cas e1' Subst {from = x, to = e2}
    _ -> App evaluated e2

eval NormalOrder (Abs x e) = Abs x $ eval NormalOrder e
eval NormalOrder (App e1 e2) = let e1' = eval CallByName e1 in
  case e1' of
    Abs x e -> eval NormalOrder $ cas e Subst {from = x, to = e2}
    _ -> let e1'' = eval NormalOrder e1' in
      let e2' = eval NormalOrder e2 in 
        App e1'' e2'

eval ApplicativeOrder (Abs x e) = Abs x $ eval ApplicativeOrder e
eval ApplicativeOrder (App e1 e2) = let e1' = eval ApplicativeOrder e1 in
  let e2' = eval ApplicativeOrder e2 in
    case e1' of
      Abs x e -> eval ApplicativeOrder $ cas e Subst {from = x, to = e2'}
      _ -> App e1' e2'

-- ДеБрауновское представление лямбда-термов
data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn deriving Eq

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show (VarDB x) = show x
  show (AbsDB x) = "λ." ++ show x
  show (AppDB a b) = show a ++ " " ++ showWithBraces b
    where
      foldIntoBraces :: String -> String
      foldIntoBraces a = "(" ++ a ++ ")"

      showWithBraces :: DeBruijn -> String
      showWithBraces (VarDB a) = show a
      showWithBraces a = foldIntoBraces $ show a

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: (Eq a, Hashable a) => Lambda a -> DeBruijn
toDeBruijn expr = computeDeBruijn expr Map.empty 0
  where
    computeDeBruijn :: (Eq a, Hashable a) => Lambda a -> Map.HashMap a Int -> Int -> DeBruijn
    computeDeBruijn (Var a) indexes depth = let index = Map.findWithDefault 0 a indexes in
      VarDB (depth - index - 1)
    computeDeBruijn (Abs a b) indexes depth = let indexes' = Map.insert a depth indexes in
      AbsDB $ computeDeBruijn b indexes' (depth + 1)
    computeDeBruijn (App a b) index depth = AppDB (computeDeBruijn a index depth) (computeDeBruijn b index depth)

-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: DeBruijn -> Lambda Int
fromDeBruijn = fromDeBruijnDepthed 0
  where
    fromDeBruijnDepthed :: Int -> DeBruijn -> Lambda Int
    fromDeBruijnDepthed depth (VarDB a) = Var $ depth - a - 1
    fromDeBruijnDepthed depth (AppDB a b) = App (fromDeBruijnDepthed depth a) (fromDeBruijnDepthed depth b)
    fromDeBruijnDepthed depth (AbsDB a) = Abs depth (fromDeBruijnDepthed (depth + 1) a)