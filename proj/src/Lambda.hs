
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Lambda where

import qualified Data.Set as Set

import Data.Maybe

data Lambda a = Var a
              | App (Lambda a) (Lambda a)
              | Abs a (Lambda a)
              deriving Eq


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
showBrackets str cond | cond = "(" ++ str ++ ")"
                      | otherwise = str

-- Красивая печать без лишних скобок.
instance MyShow a => Show (Lambda a) where
  show (Var a) = _show a
  show (Abs x term) = "λ" ++ (_show x) ++ "." ++ (show term)
  show (App a b) = (showBrackets (show a) (isAbs a)) ++ " " ++ (showBrackets (show b) (isNotVar b))


data DeBruijn = VarDB Int
              | AbsDB DeBruijn
              | AppDB DeBruijn DeBruijn
              deriving Eq

getFreeVariables :: Eq a => Ord a => Lambda a -> Set.Set a
getFreeVariables (Var a) = Set.singleton a
getFreeVariables (App f g) = (getFreeVariables f) `Set.union` (getFreeVariables g)
getFreeVariables (Abs x f) = Set.delete x (getFreeVariables f)

toDeBruijn :: Eq a => Ord a => Lambda a -> DeBruijn
toDeBruijn term = helpfunction term initialList 0
  where helpfunction :: Eq a => Ord a => Lambda a -> [(a, Int)] -> Int -> DeBruijn
        helpfunction (Var x) lst h = VarDB (subtract y h)
          where y = (fromJust (lookup x lst))
        helpfunction (App f g) lst h  = AppDB (helpfunction f lst h) (helpfunction g lst h)
        helpfunction (Abs x g) lst h = AbsDB (helpfunction g ((x, h) : lst) (succ h))
        initialList = zip (Set.toList (getFreeVariables term)) ([-1, -2..])

alphaEq :: Ord a => Eq a => Lambda a -> Lambda a -> Bool
alphaEq first second = (toDeBruijn first) == (toDeBruijn second)