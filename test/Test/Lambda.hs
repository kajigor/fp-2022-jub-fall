module Test.Lambda where

import Lambda
import Test.Tasty.HUnit

unit_show :: IO ()
unit_show = do
   show Lambda.true @?= "\\x.\\y.x"
   show Lambda.successor @?= "\\n.\\f.\\x.f(n f x) "
   show Lambda.ifThenElse @?= "\\p.\\a.\\b.p a b"
   show Lambda.and @?= "\\p.\\q.p q p"