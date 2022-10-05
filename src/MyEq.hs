{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}
module MyEq where

-- Класс типа, эквивалентный Eq из стандартной библиотеки
class MyEq a where
  (===)  :: a -> a -> Bool
  (=/=) :: a -> a -> Bool

  -- Дефолтные реализации для функций.
  -- Достаточно реализовать одну, и вторая будет определяться через первую.
  (=/=) x y = not $ (===) x y
  (===) x y = not $ (=/=) x y

-- Сравнение на равенство примитивного типа Bool
instance MyEq Bool where
  (===) :: Bool -> Bool -> Bool
  (===) True  x = x
  (===) False x = not x

  -- -- Можно определить, как сравнивать Bool-ы на неравенство,
  -- -- но это не обязательно, так как определение совпадает с дефолтным
  -- (=/=) :: Bool -> Bool -> Bool
  -- (=/=) True  x = not x
  -- (=/=) False x = x

-- Сравнение на равенство для целых чисел.
-- Пользуемся тем, что Int является инстансом Eq,
-- а значит можно воспользоваться равенством для Int
instance MyEq Int where
  (===) :: Int -> Int -> Bool
  (===) = (==)

-- Как сравнивать на равенство пары значений
instance (MyEq a, MyEq b) => MyEq (a, b) where
  (===) :: (a, b) -> (a, b) -> Bool
  (===) (x0, y0) (x1, y1) = x0 === x1 && y0 === y1
