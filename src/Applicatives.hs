{-# LANGUAGE InstanceSigs #-}
module Applicatives where

class MyFunctor f where
  myFmap :: (a -> b) -> f a -> f b

-- Законы:
-- fmap id == id
-- fmap (f . g) == fmap f . fmap g

instance MyFunctor [] where
  myFmap :: (a -> b) -> [a] -> [b]
  myFmap _ [] = []
  myFmap f (x : xs) = f x : myFmap f xs

instance MyFunctor Maybe where
  myFmap :: (a -> b) -> Maybe a -> Maybe b
  myFmap _ Nothing = Nothing
  myFmap f (Just x) = Just $ f x

instance MyFunctor (Either e) where
  myFmap :: (a -> b) -> Either e a -> Either e b
  myFmap _ (Left e) = Left e
  myFmap f (Right x) = Right $ f x

instance MyFunctor ((,) s) where
  myFmap :: (a -> b) -> (s, a) -> (s, b)
  myFmap f (x,y) = (x, f y)

-- (->) a b === a -> b
instance MyFunctor ((->) e) where
  myFmap :: (a -> b) -> (e -> a) -> ((->) e) b
  -- myFmap :: (a -> b) -> (e -> a) -> (e -> b)
  -- myFmap :: (a -> b) -> (e -> a) -> (e -> b)
  -- myFmap f g = \e -> f (g e)
  myFmap = (.)

    -- f :: (a -> b)
    -- g :: (e -> a)
    -- e :: e
    -- g e :: a

-- (f <$> g) <$> xs = (f <$> g) <$> xs = (f . g) <$> xs = fmap (f . g) xs
-- f <$> (g <$> xs) = fmap f (fmap g xs)

-- fmap2 :: (a -> b -> r) -> f a -> f b -> f r
-- fmap3 :: (a -> b -> c -> r) -> f a -> f b -> f c -> f r

-- fmap2 g as bs
-- as :: f a
-- bs :: f b
-- g :: a -> (b -> r)

-- fmap2 :: (a -> (b -> r)) -> f a -> f b -> f r
-- fmap2 g :: f a -> f b -> f r
-- fmap2 g as :: f b -> f r

-- fmap :: (c -> d) -> f c -> f d
-- g :: a -> (b -> r)
-- fmap g :: f a -> f (b -> r)
-- fmap g as :: f (b -> r)

-- ap :: f (a -> b) -> f a -> f b

-- fmap2 g as bs = fmap g as `ap` bs
-- fmap3 g as bs cs = (fmap g as `ap` bs) `ap` cs

class MyFunctor f => MyApplicative f where
  -- В Applicative функция называется pure
  myPure :: a -> f a

  -- В Applicative функция называется <*>
  (>*<) :: f (a -> b) -> f a -> f b


instance MyApplicative Maybe where
  myPure :: a -> Maybe a
  myPure x = Just x

  (>*<) :: Maybe (a -> b) -> Maybe a -> Maybe b
  Nothing >*< _ = Nothing
  (Just g) >*< x = myFmap g x
  -- (Just g) >*< (Just x) = Just $ g x
  -- (Just g) >*< Nothing = Nothing

instance MyApplicative [] where
  myPure :: a -> [a]
  myPure x = [x]

  (>*<) :: [a -> b] -> [a] -> [b]
  gs >*< xs = [ g x | g <- gs, x <- xs ]

newtype ZipList a = ZipList [a] deriving (Show)

instance MyFunctor ZipList where
  myFmap :: (a -> b) -> ZipList a -> ZipList b
  myFmap f (ZipList xs) = ZipList $ myFmap f xs

instance MyApplicative ZipList where
  myPure :: a -> ZipList a
  myPure x = ZipList $ repeat x

  (>*<) :: ZipList (a -> b) -> ZipList a -> ZipList b
  ZipList fs >*< ZipList xs = ZipList (zipWith ($) fs xs)
