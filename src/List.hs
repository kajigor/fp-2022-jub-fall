module List where

-- cross :: [a] -> [b] -> [(a, b)]
-- cross xs ys = do
--   x <- xs
--   y <- ys
--   return (x, y)

-- instance Monad [] where
--   (>>=) :: [a] -> (a -> [b]) -> [b]
--   xs >>= f =
--     case xs of
--       (h : t) -> f h ++ t >>= f
--       [] ->  []
--   -- xs >>= f
--   --    xs >>= f = [y | x <- xs, y <- f x]