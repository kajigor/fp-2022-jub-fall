{-# LANGUAGE GADTs #-}

module List where

-- data Maybe' a
--   = Nothing'
--   | Just' a

data Maybe' a where
  Nothing' :: Maybe' a
  Just' :: a -> Maybe' a

-- data List a
--   = Nil
--   | Cons a (List a)

data Z   -- zero
data S n -- succ

-- type family

data List a s where
  Nil :: List a Z
  Cons :: a -> List a n -> List a (S n)

-- head :: List a s -> a
-- head (Cons x xs) = x

safeHead :: List a (S n) -> a
safeHead (Cons x _) = x

-- t = safeHead Nil

safeZip :: List a n -> List b n -> List (a, b) n
safeZip (Cons x xs) (Cons y ys) = Cons (x, y) (safeZip xs ys)
safeZip Nil Nil = Nil

-- type literal

instance Show a => Show (List a s) where
  show (Cons x xs) = show x ++ " : " ++ show xs
  show Nil = "[]"

