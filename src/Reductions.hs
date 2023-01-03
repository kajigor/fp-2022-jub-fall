{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Reductions where

import Data.Char
import Lambda
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative
import Data.Void
    

newtype Reductor x = Reductor {
    reduce :: (Maybe (Lambda String)) -> Maybe (Maybe (Lambda String), x)
}

instance Functor Reductor where
  fmap :: (a -> b) -> Reductor a -> Reductor b
  fmap f (Reductor x) = Reductor $ \inp -> do
    (y, k) <- x inp
    return (y, f k)
   
instance Applicative Reductor where
  pure :: a -> Reductor a
  pure x = Reductor $ \inp -> Just (inp, x)

  (<*>) :: Reductor (a -> b) -> Reductor a -> Reductor b
  Reductor x <*> Reductor y = Reductor $ \xs -> do
    (ys, g) <- x xs
    (ys', k) <- y ys
    return (ys', g k)

instance Alternative Reductor where
  empty :: Reductor a
  empty = Reductor $ const Nothing

  (<|>) :: Reductor a -> Reductor a -> Reductor a
  Reductor x <|> Reductor y = Reductor $ \xs ->
    case x xs of
      Nothing -> y xs
      z -> z


getApp1 :: Reductor (Lambda String)
getApp1 = Reductor get 
    where
        get (Just (App a b)) = Just (Just a, b)
        get _ = Nothing

getApp2 :: Reductor (Lambda String)
getApp2 = Reductor get 
    where
        get (Just (App a b)) = Just (Just b, a)
        get _ = Nothing        
      

getAbs :: Reductor String
getAbs = Reductor get
    where 
        get (Just (Abs a b)) = Just ((Just b), a)
        get _ = Nothing

makeApp :: Lambda String -> Lambda String -> Lambda String
makeApp x y = App y x

makeSubst :: Reductor (Lambda String)
makeSubst = Reductor get
    where 
        get (Just (App (Abs x a) b)) = Just (Nothing, cas a (Subst x b))
        get _ = Nothing

reduceCallByValue :: Reductor (Lambda String)
reduceCallByValue = (makeApp <$> getApp1 <*> reduceCallByValue) <|>
                    (App <$> getApp2 <*> reduceCallByValue) <|>
                    makeSubst

reduceCallByName :: Reductor (Lambda String)
reduceCallByName = (makeApp <$> getApp1 <*> reduceCallByName) <|>
                   makeSubst

reduceNormalOrder :: Reductor (Lambda String)
reduceNormalOrder = (makeApp <$> getApp1 <*> reduceCallByName) <|>
                    makeSubst <|>          
                    (makeApp <$> getApp1 <*> reduceNormalOrder) <|> 
                    (App <$> getApp2 <*> reduceNormalOrder) <|>
                    (Abs <$> getAbs <*> reduceNormalOrder)       

reduceApplicativeOrder :: Reductor (Lambda String)
reduceApplicativeOrder = (makeApp <$> getApp1 <*> reduceApplicativeOrder) <|>        
                         (App <$> getApp2 <*> reduceApplicativeOrder) <|> 
                         (makeSubst) <|>  
                         ((\x -> \y -> (Abs x y)) <$> getAbs <*> reduceApplicativeOrder)    

getReduce :: Strategy -> Reductor (Lambda String)
getReduce CallByName = reduceCallByName
getReduce CallByValue = reduceCallByValue
getReduce NormalOrder = reduceNormalOrder
getReduce ApplicativeOrder = reduceApplicativeOrder

reduceOnce :: Strategy -> Lambda String  -> Maybe (Lambda String)
reduceOnce strat lamb =  do
    next <- reduce (getReduce strat) (Just lamb)
    return (snd next)

reductionList :: Strategy -> Lambda String -> [Lambda String] -> Maybe ([Lambda String])
reductionList strat lamb cur_lst = if (length cur_lst) > 500 then Nothing
                                   else case new_term of
                                        Nothing -> Just (cur_lst)
                                        Just x -> reductionList strat x (cur_lst ++ [x])
                                        where 
                                            new_term = reduceOnce strat lamb

