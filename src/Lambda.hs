{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lambda where

import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as Set

-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda
  = Var String
  | App Lambda Lambda
  | Abs String Lambda
  deriving (Eq)

condWrap :: Bool -> String -> String
condWrap True s = "(" ++ s ++ ")"
condWrap False s = s

isApp :: Lambda -> Bool
isApp e = case e of
  App _ _ -> True
  _ -> False

isAbs :: Lambda -> Bool
isAbs e = case e of
  Abs _ _ -> True
  _ -> False

-- Красивая печать без лишних скобок.
instance Show Lambda where
  show expr = case expr of
    Var s -> s
    App x y ->
      condWrap (isAbs x) (show x) ++ " "
        ++ condWrap (isApp y || isAbs y) (show y)
    Abs x y -> "λ" ++ x ++ "." ++ show y

class Ord a => Fresh a where
  candidates :: [a]

  fresh :: Set.Set a -> a
  fresh used = head $ filter isFree candidates
    where
      isFree x = x `Set.notMember` used

instance Fresh String where
  candidates = [f n | n <- [0 ..]]
    where
      alpha = 26
      f n
        | n < alpha = [chr (n + ord 'a')]
        | otherwise = f (n `div` alpha) ++ f (n `rem` alpha)

freeVariables :: Lambda -> Set.Set String
freeVariables (Var x) = Set.singleton x
freeVariables (App x y) = freeVariables x `Set.union` freeVariables y
freeVariables (Abs x y) = x `Set.delete` freeVariables y

getVariables :: Lambda -> Set.Set String
getVariables (Var x) = Set.singleton x
getVariables (App x y) = getVariables x `Set.union` getVariables y
getVariables (Abs x y) = x `Set.insert` freeVariables y

-- ДеБрауновское представление лямбда-термов
data DeBruijn
  = VarDB Int
  | AppDB DeBruijn DeBruijn
  | AbsDB DeBruijn
  deriving (Eq)

isAppDB :: DeBruijn -> Bool
isAppDB e = case e of
  AppDB _ _ -> True
  _ -> False

isAbsDB :: DeBruijn -> Bool
isAbsDB e = case e of
  AbsDB _ -> True
  _ -> False

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show expr = case expr of
    VarDB s -> show s
    AppDB x y ->
      condWrap (isAbsDB x) (show x) ++ " "
        ++ condWrap (isAppDB y || isAbsDB y) (show y)
    AbsDB y -> "λ " ++ show y

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Lambda -> DeBruijn
toDeBruijn term = fromJust (f fv term)
  where
    fv = Set.toList $ freeVariables term
    f s (Var x) = do
      i <- elemIndex x s
      return $ VarDB (i + 1)
    f s (App x y) = do
      x' <- f s x
      y' <- f s y
      return $ AppDB x' y'
    f s (Abs x y) = do
      y' <- f (x : s) y
      return $ AbsDB y'

sizeDeBruijn :: DeBruijn -> Int
sizeDeBruijn (VarDB _) = 1
sizeDeBruijn (AppDB x y) = sizeDeBruijn x + sizeDeBruijn y
sizeDeBruijn (AbsDB y) = sizeDeBruijn y

-- Преобразовать деБрауновские лямбда-термы в обычные.
fromDeBruijn :: DeBruijn -> Lambda
fromDeBruijn term = f (take (sizeDeBruijn term) candidates) term
  where
    f :: [String] -> DeBruijn -> Lambda
    f stack (VarDB x) = Var $ fromJust $ listToMaybe $ drop (x - 1) stack
    f stack (AppDB x y) = App (f stack x) (f stack y)
    f stack (AbsDB y) = Abs x (f (x : stack) y)
      where
        x = fresh $ Set.fromList stack

data VarName = Bound Int | Free String deriving Eq

instance Show VarName where
  show (Bound x) = show x
  show (Free x) = x

data LocallyNameless
  = VarLN VarName
  | AppLN LocallyNameless LocallyNameless
  | AbsLN LocallyNameless
  deriving Eq

isAppLN :: LocallyNameless -> Bool
isAppLN e = case e of
  AppLN _ _ -> True
  _ -> False

isAbsLN :: LocallyNameless -> Bool
isAbsLN e = case e of
  AbsLN _ -> True
  _ -> False

instance Show LocallyNameless where
  show expr = case expr of
    VarLN s -> show s
    AppLN x y ->
      condWrap (isAbsLN x) (show x) ++ " "
        ++ condWrap (isAppLN y || isAbsLN y) (show y)
    AbsLN y -> "λ " ++ show y

toLocallyNameless :: Lambda -> LocallyNameless
toLocallyNameless = f []
  where
    f :: [String] -> Lambda -> LocallyNameless
    f s (Var x) = VarLN $ maybe (Free x) (Bound . (+1)) (elemIndex x s)
    f s (App x y) = AppLN (f s x) (f s y)
    f s (Abs x y) = AbsLN (f (x : s) y)

fromLocallyNameless :: LocallyNameless -> Lambda
fromLocallyNameless = f []
  where
    f :: [String] -> LocallyNameless -> Lambda
    f stack (VarLN (Bound x)) = Var $ fromJust $ listToMaybe $ drop (x - 1) stack
    f stack (VarLN (Free x)) = Var x
    f stack (AppLN x y) = App (f stack x) (f stack y)
    f stack (AbsLN y) = Abs x (f (x : stack) y)
      where
        x = fresh $ Set.fromList stack

-- Проверка термов на альфа-эквивалентность.
alphaEq :: Lambda -> Lambda -> Bool
alphaEq = (==) `on` toDeBruijn