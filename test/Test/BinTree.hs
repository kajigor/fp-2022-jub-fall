module Test.BinTree where

import Test.Tasty.HUnit (Assertion, (@?=))
import Control.Monad.Trans.State.Lazy

import BinTree

genTree :: Enum a => Int -> a -> BinTree a
genTree = evalState . go
  where
    go n | n <= 1 = do
      h <- get
      put (succ h)
      return $ Leaf h
    go n = do
      h <- get
      put $ succ h
      l <- go (n - 1)
      r <- go (n - 1)
      return $ Node h l r

unit_root = do
  root (genTree 1 0) @?= 0
  root (genTree 13 0) @?= 0
  root (genTree 42 (-7)) @?= (-7)

unit_leaves = do
  leaves (genTree 1 0) @?= [0]
  leaves (genTree 2 0) @?= [1,2]
  leaves (genTree 3 0) @?= [2,3,5,6]
  leaves (genTree 4 (-7)) @?= [-4,-3,-1,0,3,4,6,7]

unit_nodes = do
  nodes (genTree 1 0) @?= [0]
  nodes (genTree 2 0) @?= [1, 0, 2]
  nodes (genTree 3 0) @?= [2,1,3,0,5,4,6]
  nodes (genTree 4 (-7)) @?= [-4,-5,-3,-6,-1,-2,0,-7,3,2,4,1,6,5,7]

unit_depth = do
  depth (genTree 1 0) @?= 1
  depth (genTree 2 0) @?= 2
  depth (genTree 3 0) @?= 3
  depth (genTree 4 (-7)) @?= 4

unit_mapTree = do
  mapTree (+1) (genTree 1 0) @?= Leaf 1
  mapTree (+1) (genTree 2 0) @?= Node 1 (Leaf 2) (Leaf 3)
  mapTree (+1) (genTree 3 0) @?= Node 1 (Node 2 (Leaf 3) (Leaf 4)) (Node 5 (Leaf 6) (Leaf 7))
  mapTree (+1) (genTree 4 (-7)) @?= Node (-6) (Node (-5) (Node (-4) (Leaf (-3)) (Leaf (-2))) (Node (-1) (Leaf 0) (Leaf 1))) (Node 2 (Node 3 (Leaf 4) (Leaf 5)) (Node 6 (Leaf 7) (Leaf 8)))

  mapTree (*2) (genTree 1 0) @?= Leaf 0
  mapTree (*2) (genTree 2 0) @?= Node 0 (Leaf 2) (Leaf 4)
  mapTree (*2) (genTree 3 0) @?= Node 0 (Node 2 (Leaf 4) (Leaf 6)) (Node 8 (Leaf 10) (Leaf 12))
  mapTree (*2) (genTree 4 (-7)) @?= Node (-14) (Node (-12) (Node (-10) (Leaf (-8)) (Leaf (-6))) (Node (-4) (Leaf (-2)) (Leaf 0))) (Node 2 (Node 4 (Leaf 6) (Leaf 8)) (Node 10 (Leaf 12) (Leaf 14)))
