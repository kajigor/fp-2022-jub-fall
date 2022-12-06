module Rand where

import System.Random

loop :: RandomGen t => t -> IO ()
loop gen = do
  let (num, newGen) = random gen
  print (num :: Int)
  loop newGen

main :: IO ()
main = do
  g <- newStdGen
  loop g

-- main :: IO ()
-- main = do
--   g <- newStdGen -- initStdGen :: MonadIO m => m StdGen
--   let nums = randoms g :: [Int]
--   let nums' = randoms g :: [Int]
--   mapM_ print (take 10 (zip nums nums'))