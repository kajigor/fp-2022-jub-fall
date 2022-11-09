module Test.Lambda where

import Lambda
import Test.Tasty.HUnit

unit_show :: IO ()
unit_show = do
   show Lambda.true @?= "\\x.\\y.x"
   show Lambda.successor @?= "\\n.\\f.\\x.f(n f x) "
   show Lambda.ifThenElse @?= "\\p.\\a.\\b.p a b"
   show Lambda.and @?= "\\p.\\q.p q p"

unit_show1 :: IO ()
unit_show1 = do
   show (toDeBruijn Lambda.true) @?= "\\.\\.0"
   show (toDeBruijn Lambda.successor) @?= "\\.\\.\\.1(0 1 2) "
   show (toDeBruijn Lambda.ifThenElse) @?= "\\.\\.\\.0 1 2"
   show (toDeBruijn Lambda.and) @?= "\\.\\.0 1 0"