module Password where

import Data.Char
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class

-- getPassword :: IO (Maybe String)
-- getPassword = do
--   s <- getLine
--   if isValid s
--   then return $ Just s
--   else return Nothing

isValid :: String -> Bool
isValid s =
  length s >= 8
  && any isAlpha s
  && any isNumber s
  && any isPunctuation s

-- askPassword :: IO ()
-- askPassword = do
--   putStrLn "Insert your new password:"
--   maybePass <- getPassword
--   case maybePass of
--     Just value -> putStrLn "Storing in database..."
--     Nothing -> putStrLn "Password is invalid."

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance Monad m => Monad (MaybeT m) where
  return  = MaybeT . return . Just

  -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
  x >>= f = MaybeT $ do
    maybeValue <- runMaybeT x
    case maybeValue of
      Nothing -> return Nothing
      Just value -> runMaybeT $ f value

instance Monad m => Applicative (MaybeT m) where
  pure = return
  (<*>) = ap

instance Monad m => Functor (MaybeT m) where
  fmap = liftM

instance Monad m => Alternative (MaybeT m) where
  empty = MaybeT $ return Nothing
  x <|> y = MaybeT $ do
    maybeValue <- runMaybeT x
    case maybeValue of
      Nothing -> runMaybeT y
      Just _ -> return maybeValue

instance Monad m => MonadPlus (MaybeT m) where
  mzero = empty
  mplus = (<|>)

instance MonadTrans MaybeT where
  lift = MaybeT . (liftM Just)

hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

getPassword :: MaybeT IO String
getPassword = do
  s <- lift getLine
  guard (isValid s) -- guard из Alternative
  return s

-- askPassword :: MaybeT IO ()
-- askPassword = do
--   lift $ putStrLn "Insert your new password:"
--   value <- getPassword
--   lift $ putStrLn "Storing in database..."

askPassword :: MaybeT IO ()
askPassword = do
  lift $ putStrLn "Insert your new password:"
  value <- msum $ repeat getPassword
  lift $ putStrLn "Storing in database..."