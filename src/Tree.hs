{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use tuple-section" #-}
module Tree where

import Control.Monad.State

import Text.Printf

data Tree a = Leaf a | Node (Tree a) (Tree a)

-- type State s a = s -> (a, s)

type WithCounter a = Int -> (a, Int)


relabel :: Tree a -> Int -> Tree (Int, a)
relabel tree i =
    evalState (go tree) i
  where
    go :: Tree a -> State Int (Tree (Int, a))
    go (Leaf x) = do
      i <- get
      put (i + 1)
      return (Leaf (i, x))
    go (Node l r) = do
      l' <- go l
      -- modify (*2)
      r' <- go r
      return (Node l' r')

-- relabel :: Tree a -> WithCounter (Tree (Int, a))
-- relabel (Leaf x) = \i -> (Leaf (i, x), i + 1)
-- relabel (Node l r) =
--   relabel l `next` \l' ->
--   relabel r `next` \r' ->
--   ret $ Node l' r'

-- next :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
-- next f g = \i ->
--   let (a, i') = f i in
--   g a i'

-- ret :: a -> WithCounter a
-- ret x = \i -> (x, i)

-- relabel :: Tree a -> Int -> (Tree (Int, a), Int)
-- relabel (Leaf x) i = (Leaf (i, x), i + 1)
-- relabel (Node l r) i = -- Node (relabel l i) (relabel r i')
--   let (l', i') = relabel l i in
--   let (r', i'') = relabel r i' in
--   (Node l' r', i'')

tree1 :: Tree String
tree1 = Node (Node (Leaf "x") (Leaf "y")) (Node (Leaf "z") (Leaf "f"))

tree1' :: Tree (Int, String)
tree1' = Node (Node (Leaf (0, "x")) (Leaf (1, "y"))) (Node (Leaf (2, "z")) (Leaf (3, "f")))

tree2 :: Tree Int
tree2 = Node (Node (Leaf 42) (Node (Leaf 7) (Leaf 13))) (Node (Node (Leaf 7) (Leaf 1)) (Leaf 1))

tree2' :: Tree (Int, Int)
tree2' = Node (Node (Leaf (0, 42)) (Node (Leaf (1,7)) (Leaf (2,13)))) (Node (Node (Leaf (3,7)) (Leaf (4,1))) (Leaf (5,1)))

toString :: Show a => Tree a -> String
toString =
    go ""
  where
    go _ (Leaf x) = printf "%s" (show x)
    go indent (Node l r) =
      printf "┐\n%s├─%s\n%s└─%s" indent (go (indent ++ "│ ") l) indent (go (indent ++ "  ") r)
