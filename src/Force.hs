{-# LANGUAGE BangPatterns #-}

module Force where

bigNumber :: Int
bigNumber = 10^6

sumN :: Int -> Int
sumN n =
  n + sumN (n - 1)
-- *Force> sumN bigNumber
-- *** Exception: stack overflow

sumNBetter :: Int -> Int
sumNBetter n =
    go 0 n
  where
    go acc 0 = acc
    go acc n = go (acc + n) (n - 1)
-- *Force> :set +s
-- *Force> sumNBetter bigNumber
-- 500000500000
-- (0.94 secs, 378,084,584 bytes)


sumNBest :: Int -> Int
sumNBest n =
    go 0 n
  where
    go acc 0 = acc
    go acc n = acc `seq` go (acc + n) (n - 1)
-- *Force> sumNBest bigNumber
-- 500000500000
-- (0.73 secs, 408,464,616 bytes)

sumNBang :: Int -> Int
sumNBang n =
    go 0 n
  where
    go acc 0 = acc
    go !acc n = go (acc + n) (n - 1)
-- *Force> sumNBang bigNumber
-- 500000500000
-- (0.67 secs, 344,464,616 bytes)

sumNApp :: Int -> Int
sumNApp n =
    go 0 n
  where
    go acc 0 = acc
    go acc n = (go $! (acc + n)) (n - 1)
-- *Force> sumNApp bigNumber
-- 500000500000
-- (0.81 secs, 464,464,616 bytes)
