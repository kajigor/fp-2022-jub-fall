module Test.Person where

import qualified Data.Set as Set
import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person

grandparent1 :: Person
grandparent1 = 
    Person { firstName = "Ivan"
           , lastName = "Ivanov"
           , formerLastNames = []
           , age = 50
           , idNumber = Just (Passport (4619, 672543))
           , parents = Set.empty
    }

grandparent2 :: Person
grandparent2 = 
    Person { firstName = "Maria"
           , lastName = "Ivanova"
           , formerLastNames = []
           , age = 48
           , idNumber = Just (Passport (5039, 540577))
           , parents = Set.empty
    }

grandparent3 :: Person
grandparent3 = 
    Person { firstName = "Petr"
           , lastName = "Petrov"
           , formerLastNames = []
           , age = 59
           , idNumber = Just (Passport (5346, 120358))
           , parents = Set.empty
    }

grandparent4 :: Person
grandparent4 = 
    Person { firstName = "Daria"
           , lastName = "Petrova"
           , formerLastNames = []
           , age = 61
           , idNumber = Just (Passport (3520, 947767))
           , parents = Set.empty
    }

parent1 :: Person
parent1 = 
    Person { firstName = "Vasily"
           , lastName = "Vasilev"
           , formerLastNames = []
           , age = 51
           , idNumber = Just (Passport (9146, 809405))
           , parents = Set.empty
    }

parent2 :: Person
parent2 = 
    Person { firstName = "Artem"
           , lastName = "Ivanov"
           , formerLastNames = []
           , age = 24
           , idNumber = Just (Passport (6099, 905955))
           , parents = Set.fromList [grandparent1, grandparent2]
    }

parent3 :: Person
parent3 = 
    Person { firstName = "Anna"
           , lastName = "Ivanova"
           , formerLastNames = []
           , age = 30
           , idNumber = Just (Passport (8910, 284189))
           , parents = Set.fromList [grandparent1, grandparent2]
    }

parent4 :: Person
parent4 = 
    Person { firstName = "Tatiana"
           , lastName = "Petrova"
           , formerLastNames = []
           , age = 35
           , idNumber = Just (Passport (2701, 124948))
           , parents = Set.fromList [grandparent3, grandparent4]
    }

parent5 :: Person
parent5 = 
    Person { firstName = "Terenty"
           , lastName = "Petrov"
           , formerLastNames = []
           , age = 30
           , idNumber = Just (Passport (6757, 504453))
           , parents = Set.fromList [grandparent3, grandparent4]
    }

parent6 :: Person
parent6 = 
    Person { firstName = "Koza"
           , lastName = "Sidorova"
           , formerLastNames = []
           , age = 23
           , idNumber = Just (Passport (2402, 678882))
           , parents = Set.empty
    }

parent7 :: Person
parent7 = 
    Person { firstName = "Alex"
           , lastName = "Shmidt"
           , formerLastNames = []
           , age = 21
           , idNumber = Just (Passport (9088, 932009))
           , parents = Set.empty
    }

child1 :: Person
child1 =
    Person { firstName = "Ivan"
           , lastName = "Vasilev"
           , formerLastNames = []
           , age = 0
           , idNumber = Nothing
           , parents = Set.fromList [parent1, parent2]
    }

child2 :: Person
child2 =
    Person { firstName = "Arina"
           , lastName = "Ivanova"
           , formerLastNames = []
           , age = 0
           , idNumber = Nothing
           , parents = Set.fromList [parent3]
    }

child3 :: Person
child3 =
    Person { firstName = "Julia"
           , lastName = "Petrova"
           , formerLastNames = []
           , age = 13
           , idNumber = Just (BirthSertificate ("IV-ЖИ", 437920))
           , parents = Set.fromList [parent4]
    }

child4 :: Person
child4 =
    Person { firstName = "Vasily"
           , lastName = "Ivanov-Petrov"
           , formerLastNames = []
           , age = 4
           , idNumber = Just (BirthSertificate ("II-ЗЧ", 255915))
           , parents = Set.fromList [parent3, parent4]
    }

child5 :: Person
child5 =
    Person { firstName = "EAX-25"
           , lastName = "Petrov"
           , formerLastNames = []
           , age = 0
           , idNumber = Nothing
           , parents = Set.fromList [parent5, parent6, parent7]
    }

people = Set.fromList [grandparent1, grandparent2, grandparent3, grandparent4, parent1, parent2, parent3
                        , parent4, parent5, parent6, parent7, child1, child2, child3, child4, child5]

tree1 = Node child5 Set.empty
tree2 = Node parent5 (Set.singleton tree1)
tree3 = Node parent4 (Set.fromList [Node child3 Set.empty, Node child4 Set.empty])
tree4 = Node grandparent3 (Set.fromList [tree2, tree3])

unit_createChild = do
  createChild (Set.fromList [parent1, parent2]) "Ivan" "Vasilev" @?= child1
  createChild (Set.fromList [parent3]) "Arina" "Ivanova" @?= child2
  createChild (Set.fromList [parent5, parent6, parent7]) "EAX-25" "Petrov" @?= child5

unit_greatestAncestor = do
    greatestAncestor child2 @?= grandparent1
    greatestAncestor child4 @?= grandparent4
    greatestAncestor parent7 @?= parent7
    greatestAncestor parent2 @?= grandparent1
    greatestAncestor child1 @?= grandparent1

unit_ancestors = do
    ancestors 0 child3 @?= Set.fromList [child3]
    ancestors 1 child3 @?= Set.fromList [parent4]
    ancestors 2 child3 @?= Set.fromList [grandparent3, grandparent4]
    ancestors 3 child3 @?= Set.empty
    ancestors 2 child4 @?= Set.fromList [grandparent1, grandparent2, grandparent3, grandparent4]

unit_descendants = do
    descendants child5 people @?= tree1
    descendants parent5 people @?= tree2
    descendants parent4 people @?= tree3
    descendants grandparent3 people @?= tree4