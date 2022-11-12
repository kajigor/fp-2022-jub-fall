module Test.Lambda where
import Test.HUnit (Assertion, assertBool, (@?=))

import Lambda

unit_show_string :: Assertion
unit_show_string = do
    (show true) @?= "$x.$y.x"
    (show false) @?= "$x.$y.y"
    (show Lambda.and) @?= "$p.$q.p q p"
    (show Lambda.or) @?= "$p.$q.p p q"
    (show Lambda.not) @?= "$p.p ($x.$y.y) ($x.$y.x)"
    (show ifThenElse) @?= "$p.$a.$b.p a b"
    (show zero) @?= "$f.$x.x"
    (show one) @?= "$f.$x.f x"
    (show four) @?= "$f.$x.f (f (f (f x)))"
