{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Lambda where

import Data.Char
import Data.Function
import Data.List
import Data.Maybe
-- import Control.Monad.State
import qualified Data.Set as Set

-- Тип для лямбда-термов.
-- Параметризуем типом переменной.
data Lambda a
  = Var a
  | App (Lambda a) (Lambda a)
  | Abs a (Lambda a)
  deriving (Eq)

class ShowProperly a where
  repr :: a -> String

instance {-# OVERLAPPABLE #-} Show a => ShowProperly a where
  repr = show

instance {-# OVERLAPS #-} ShowProperly String where
  repr = id

condWrap :: Bool -> String -> String
condWrap True s = "(" ++ s ++ ")"
condWrap False s = s

isApp :: Lambda a -> Bool
isApp e = case e of
  App _ _ -> True
  _ -> False

isAbs :: Lambda a -> Bool
isAbs e = case e of
  Abs _ _ -> True
  _ -> False

-- Красивая печать без лишних скобок.
instance ShowProperly a => Show (Lambda a) where
  show expr = case expr of
    Var s -> repr s
    App x y ->
      condWrap (isAbs x) (show x) ++ " "
        ++ condWrap (isApp y || isAbs y) (show y)
    Abs x y -> "\\" ++ repr x ++ "." ++ show y

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

freeVariables :: Ord a => Lambda a -> Set.Set a
freeVariables (Var x) = Set.singleton x
freeVariables (App x y) = freeVariables x `Set.union` freeVariables y
freeVariables (Abs x y) = x `Set.delete` freeVariables y

getVariables :: Ord a => Lambda a -> Set.Set a
getVariables (Var x) = Set.singleton x
getVariables (App x y) = freeVariables x `Set.union` freeVariables y
getVariables (Abs x y) = x `Set.insert` freeVariables y



isAppDB :: DeBruijn -> Bool
isAppDB e = case e of
  AppDB _ _ -> True
  _ -> False

isAbsDB :: DeBruijn -> Bool
isAbsDB e = case e of
  AbsDB _ -> True
  _ -> False

-- ДеБрауновское представление лямбда-термов
data DeBruijn
  = VarDB Int
  | AppDB DeBruijn DeBruijn
  | AbsDB DeBruijn
  deriving (Eq)

-- Красивая печать без лишних скобок.
instance Show DeBruijn where
  show expr = case expr of
    VarDB s -> show s
    AppDB x y ->
      condWrap (isAbsDB x) (show x) ++ " "
        ++ condWrap (isAppDB y || isAbsDB y) (show y)
    AbsDB y -> "\\ " ++ show y

-- λx. λy. x ≡ λ λ 2
-- λx. λy. λz. x z (y z) ≡ λ λ λ 3 1 (2 1)
-- λz. (λy. y (λx. x)) (λx. z x) ≡ λ (λ 1 (λ 1)) (λ 2 1)

-- Преобразовать обычные лямбда-термы в деБрауновские
toDeBruijn :: Ord a => Lambda a -> DeBruijn
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
fromDeBruijn :: Fresh a => DeBruijn -> Lambda a
fromDeBruijn term = f (take (sizeDeBruijn term) candidates) term
  where
    f :: Fresh a => [a] -> DeBruijn -> Lambda a
    f stack (VarDB x) = Var $ fromJust $ listToMaybe $ drop (x - 1) stack
    f stack (AppDB x y) = App (f stack x) (f stack y)
    f stack (AbsDB y) = Abs x (f (x : stack) y)
      where
        x = fresh $ Set.fromList stack
      
-- Проверка термов на альфа-эквивалентность.
alphaEq :: Ord a => Lambda a -> Lambda a -> Bool
alphaEq = (==) `on` toDeBruijn