{-# LANGUAGE InstanceSigs #-}
module List where

import MyEq (MyEq (..))

data List a = Nil | Cons a (List a)

-- Имея значения, которые можно сравнивать на равенство,
-- можно определить, как сравнивать на равенство списки таких значений
instance MyEq a => MyEq (List a) where
  (===) :: List a -> List a -> Bool
  (===) Nil Nil = True
  (===) (Cons x xs) (Cons y ys) = x === y && xs === ys
  (===) _ _ = False