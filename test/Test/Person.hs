{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module Test.Person where

import Test.Tasty.HUnit (assertBool)
import Person
import qualified Data.Set as Set
import qualified Data.Tree as Tree

parentsFather :: Person -> Parents
parentsFather f = Parents { mother = Nothing, father = Just f }

parentsMother :: Person -> Parents
parentsMother m = Parents { mother = Just m, father = Nothing }

parentsInit :: Person -> Person -> Parents
parentsInit m f = Parents { mother = Just m, father = Just f }

jaehaerys :: Person
jaehaerys = Person
    { firstName = "Jaehaerys"
    , lastName = "Targaryen"
    , formerLastNames = []
    , age = 100
    , idNumber = Passport (1, 1)
    , parents = NoParents
    }

baelon :: Person
baelon = Person
    { firstName = "Baelon"
    , lastName = "Targaryen"
    , formerLastNames = []
    , age = 83
    , idNumber = Passport (2, 1)
    , parents = parentsFather jaehaerys
    }

aemon :: Person
aemon = Person
    { firstName = "Aemon"
    , lastName = "Targaryen"
    , formerLastNames = []
    , age = 81
    , idNumber = Passport (2, 2)
    , parents = parentsFather jaehaerys
    }

otto :: Person
otto = Person
    { firstName = "Otto"
    , lastName = "Hightower"
    , formerLastNames = []
    , age = 45
    , idNumber = Passport (3, 1)
    , parents = NoParents
    }

viserys :: Person
viserys = Person
    { firstName = "Viserys"
    , lastName = "Targaryen"
    , formerLastNames = []
    , age = 62
    , idNumber = Passport (3, 2)
    , parents = parentsFather baelon
    }

daemon :: Person
daemon = Person
    { firstName = "Daemon"
    , lastName = "Targaryen"
    , formerLastNames = []
    , age = 37
    , idNumber = Passport (3, 3)
    , parents = parentsFather baelon
    }

rhaenys :: Person
rhaenys = Person
    { firstName = "Rhaenys"
    , lastName = "Velaryon"
    , formerLastNames = ["Targaryen"]
    , age = 51
    , idNumber = Passport (3, 4)
    , parents = parentsFather aemon
    }

corlys :: Person
corlys = Person
    { firstName = "Corlys"
    , lastName = "Velaryon"
    , formerLastNames = []
    , age = 62
    , idNumber = Passport (3, 5)
    , parents = NoParents
    }

alicent :: Person
alicent = Person
    { firstName = "Alicent"
    , lastName = "Targaryen"
    , formerLastNames = ["Hightower"]
    , age = 20
    , idNumber = Passport (4, 1)
    , parents = parentsFather otto
    }

rhaenyra :: Person
rhaenyra = Person
    { firstName = "Rhaenyra"
    , lastName = "Targaryen"
    , formerLastNames = []
    , age = 23
    , idNumber = Passport (4, 2)
    , parents = parentsFather viserys
    }

laena :: Person
laena = Person
    { firstName = "Laena"
    , lastName = "Velaryon"
    , formerLastNames = []
    , age = 15
    , idNumber = Passport (4, 3)
    , parents = parentsInit rhaenys corlys
    }

laenor :: Person
laenor = Person
    { firstName = "Laenor"
    , lastName = "Velaryon"
    , formerLastNames = []
    , age = 15
    , idNumber = Passport (4, 4)
    , parents = parentsInit rhaenys corlys
    }

aegon :: Person
aegon = Person
    { firstName = "Aegon"
    , lastName = "Targaryen"
    , formerLastNames = []
    , age = 3
    , idNumber = BirthCert (5, 1)
    , parents = parentsInit alicent viserys
    }

rdchild :: Person
rdchild = Person
    { firstName = "Unnamed"
    , lastName = "Targaryen"
    , formerLastNames = []
    , age = 0
    , idNumber = BirthCert (5, 2)
    , parents = parentsInit rhaenyra daemon
    }

unit_createChild :: IO ()
unit_createChild = do
    let child1 = Person {
          firstName = "child1"
        , lastName = "lname"
        , formerLastNames  = []
        , age = 0
        , idNumber = BirthCert (0, 1)
        , parents = NoParents
        }
    let child2 = Person {
          firstName = "child2"
        , lastName = "lname"
        , formerLastNames  = []
        , age = 0
        , idNumber = BirthCert (0, 2)
        , parents = parentsMother child1
        }

    assertBool "createChild" $ createChild "child1" "lname" (BirthCert (0, 1)) NoParents == child1
    assertBool "createChild" $ createChild "child2" "lname" (BirthCert (0, 2)) (parentsMother child1) == child2


unit_greatestAncestor :: IO ()
unit_greatestAncestor = do
    assertBool "Otto Hightower" $ greatestAncestor alicent == otto
    assertBool "Jaehaerys Targaryen" $ greatestAncestor laena == jaehaerys
    assertBool "Jaehaerys Targaryen" $ greatestAncestor jaehaerys == jaehaerys

unit_ancestors :: IO ()
unit_ancestors = do
    let rhaenyra3 = Set.singleton jaehaerys
    let alicent1 = Set.singleton otto
    let rdchild1 = Set.fromList [rhaenyra, daemon]
    let aegon2 = Set.fromList [otto, baelon]
    let laenor1 = Set.fromList [rhaenys, corlys]
    assertBool "rhaenyra3" $ ancestors 3 rhaenyra == rhaenyra3
    assertBool "alicent1" $ ancestors 1 alicent == alicent1
    assertBool "rdchild1" $ ancestors 1 rdchild == rdchild1
    assertBool "aegon2" $ ancestors 2 aegon == aegon2
    assertBool "laenor1" $ ancestors 1 laenor == laenor1

unit_descendants :: IO ()
unit_descendants = do
    -- Because of the details of the implementation nodes need to be sorted by their respective values
    -- Otherwise the tests fail for people whose children are not sorted
    -- For example if we change viserys_d to Tree.Node viserys [aegon_d, alicent_d], the tests fail for Viserys and all his ancestors
    let aegon_d =     Tree.Node aegon []
    let rdchild_d =   Tree.Node rdchild []
    let laena_d =     Tree.Node laena []
    let laenor_d =    Tree.Node laenor []
    let alicent_d =   Tree.Node alicent [aegon_d]
    let rhaenyra_d =  Tree.Node rhaenyra [rdchild_d]
    let rhaenys_d =   Tree.Node rhaenys [laena_d, laenor_d]
    let corlys_d =    Tree.Node corlys [laena_d, laenor_d]
    let otto_d =      Tree.Node otto [alicent_d]
    -- Doesn't work:
    -- let viserys_d =   Tree.Node viserys [aegon_d, rhaenyra_d]
    let viserys_d =   Tree.Node viserys [rhaenyra_d, aegon_d]
    let daemon_d =    Tree.Node daemon [rdchild_d]
    let baelon_d =    Tree.Node baelon [viserys_d, daemon_d]
    let aemon_d =     Tree.Node aemon [rhaenys_d]
    let jaehaerys_d = Tree.Node jaehaerys [baelon_d, aemon_d]
    let allp = Set.fromList [aegon, rdchild, laena, laenor, alicent, rhaenyra, rhaenys, corlys, otto, viserys, daemon, baelon, aemon, jaehaerys]
    assertBool "Jaeharys" $ descendants jaehaerys allp == jaehaerys_d
    assertBool "Viserys" $ descendants viserys allp == viserys_d
    assertBool "Otto" $ descendants otto allp == otto_d
    assertBool "Aegon" $ descendants aegon allp == aegon_d
