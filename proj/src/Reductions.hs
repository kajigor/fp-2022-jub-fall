{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module Reductions where

import Lambda
import Data.Maybe
import Data.List
import Data.Tuple
import Data.Char
import Control.Applicative
import qualified Data.Set as Set

class Ord a => Freshable a where 
  getFresh :: Set.Set a -> a
  available :: [a]
  getFresh st = (head $ filter notInLst available)
    where notInLst x = x `Set.notMember` st

instance Freshable String where
  available = [(gen x) | x <- [1..]]
    where gen x | x <= 26 = [chr((x - 1) + (ord 'a'))]
                | otherwise = (gen (((x - 1) `mod` 26) + 1)) ++ (gen ((x - 1) `div` 26))

data Subst a = SubPair a (Lambda a)

getVars :: Ord a => Lambda a -> Set.Set a
getVars (Var x) = Set.singleton x
getVars (App a b) = (getVars a) `Set.union` (getVars b)
getVars (Abs x a) = Set.insert x (getVars a)

cas_with_closed :: Ord a => Freshable a => Lambda a -> Subst a -> Set.Set a -> Lambda a
cas_with_closed (Var a) (SubPair x subst) closed | (a == x) = subst
                                                 | otherwise = (Var a)
cas_with_closed (App a b) subst closed = (App (cas_with_closed a subst closed) (cas_with_closed b subst closed)) 
cas_with_closed (Abs x a) subst closed = (Abs y (cas_with_closed (cas_with_closed a (SubPair x (Var y)) (new_closed)) subst (new_closed)))
  where y = (getFresh closed)
        new_closed = Set.insert y closed

data Strategy = CallByValue | CallByName | NormalOrder | ApplicativeOrder
                deriving (Show, Read)


reductor :: Strategy -> Lambda String -> Maybe (Lambda String)
reductor CallByValue lambda = do
  ans <- (runBuilder reductorCallByValue (Just lambda))
  return (snd ans)
reductor CallByName lambda = do
  ans <- (runBuilder reductorCallByName (Just lambda))
  return (snd ans)
reductor NormalOrder lambda = do 
  ans <- (runBuilder reductorNormal (Just lambda)) 
  return (snd ans)
reductor ApplicativeOrder lambda = do
  ans <- (runBuilder reductorApplicative (Just lambda))
  return (snd ans)

newtype Builder a = Builder { runBuilder :: (Maybe (Lambda String)) -> Maybe (Maybe (Lambda String), a) }

instance Functor Builder where
  fmap :: (a -> b) -> Builder a -> Builder b
  fmap f (Builder p) = Builder $ \input ->
    case p input of
      Just (input', res) -> Just (input', f res)
      Nothing -> Nothing

instance Applicative Builder where
  pure :: a -> Builder a
  pure x = Builder $ \input -> Just (input, x)

  (<*>) :: Builder (a -> b) -> Builder a -> Builder b
  Builder u <*> Builder v = Builder $ \xs ->
    case u xs of
      Nothing -> Nothing
      Just (xs', g) ->
        case v xs' of
          Nothing -> Nothing
          Just (xs'', x) -> Just (xs'', g x)

instance Alternative Builder where
  empty :: Builder a
  empty = Builder $ const Nothing

  (<|>) :: Builder a -> Builder a -> Builder a
  Builder u <|> Builder v = Builder $ \xs ->
    case u xs of
      Nothing -> v xs
      z -> z

unpack_lambda :: Builder ((Lambda String), (Lambda String))
unpack_lambda = Builder f where 
  f (Just (App (Abs x a) b)) = Just (Nothing, ((Abs x a), b))
  f _ = Nothing

unpack_first :: Builder (Lambda String)
unpack_first = Builder f where 
  f (Just (App a b)) = Just ((Just a), b)
  f _ = Nothing

unpack_second :: Builder (Lambda String)
unpack_second = Builder f where 
  f (Just (App a b)) = Just ((Just b), a)
  f _ = Nothing

into_lambda :: Builder String
into_lambda = Builder f where
  f (Just (Abs x a)) = Just ((Just a), x)
  f _ = Nothing

cas_red :: ((Lambda String), (Lambda String)) -> (Lambda String)
cas_red ((Abs x first), second) = (cas_with_closed first (SubPair x second) ((getVars (Abs x first)) `Set.union` (getVars second)))

reductorNormal :: Builder (Lambda String)
reductorNormal = (((\x -> \y -> (App y x))) <$> unpack_first <*> reductorCallByName)
            <|> (cas_red <$> unpack_lambda) 
            <|> (((\x -> \y -> (App y x))) <$> unpack_first <*> reductorNormal)
            <|> (App <$> unpack_second <*> reductorNormal)
            <|> (((\x -> \y -> (Abs x y))) <$> into_lambda <*> reductorNormal)

reductorCallByName :: Builder (Lambda String)
reductorCallByName = (((\x -> \y -> (App y x))) <$> unpack_first <*> reductorCallByName)  
            <|> (cas_red <$> unpack_lambda) 

reductorCallByValue :: Builder (Lambda String)
reductorCallByValue = (((\x -> \y -> (App y x))) <$> unpack_first <*> reductorCallByValue)
            <|> (App <$> unpack_second <*> reductorCallByValue)
            <|> (cas_red <$> unpack_lambda) 

reductorApplicative :: Builder (Lambda String)
reductorApplicative = (((\x -> \y -> (App y x))) <$> unpack_first <*> reductorApplicative)
            <|> (App <$> unpack_second <*> reductorApplicative)
            <|> (cas_red <$> unpack_lambda) 
            <|> (((\x -> \y -> (Abs x y))) <$> into_lambda <*> reductorApplicative)

reduce_list :: Strategy -> Lambda.Lambda String -> [Lambda.Lambda String] -> [Lambda.Lambda String]
reduce_list strategy term lst = (term_reduced term1)
    where term1 = reductor strategy term
          term_reduced (Just term_new) = (reduce_list strategy term_new (lst ++ [term]))
          term_reduced Nothing = lst ++ [term] 