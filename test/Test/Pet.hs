module Test.Pet where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Pet

unit_sortPets = do
  sortPets [pet1_1,
            pet2_1, pet2_2, pet2_3,
            pet3_1, pet3_2, pet3_3,
            pet4_1, pet4_2, pet4_3,
            pet5_1, pet5_2,
            pet6_1, pet6_2, pet6_3, pet6_4,
            pet7_1, pet7_2, pet7_3, pet7_4,
            pet8_1, pet8_2, pet8_3
           ] @?= [Pet { name = "Peter"
                      , owner = Person { firstName = "Ivan"
                                       , lastName = "Ivanov"
                                       , identification = Identification 9999999999}
                      , species = Dog}
                 ,Pet { name = "Roger"
                      , owner = Person { firstName = "Ivan"
                                       , lastName = "Ivanov"
                                       , identification = Identification 9999999999}
                      , species = Cat}
                 ,Pet { name = "Brer"
                      , owner = Person { firstName = "Ivan"
                                       , lastName = "Ivanov"
                                       , identification = Identification 9999999999}
                      , species = Bunny}
                 ,Pet { name = "Bugs"
                      , owner = Person { firstName = "Ivan"
                                       , lastName = "Ivanov"
                                       , identification = Identification 9999999999}
                      , species = Tarantula}
                 ,Pet { name = "Thumper"
                      , owner = Person { firstName = "Petr"
                                       , lastName = "Ivanov"
                                       , identification = Identification 9999999992}
                      , species = Dog}
                 ,Pet { name = "Thumper"
                      , owner = Person { firstName = "Petr"
                                       , lastName = "Ivanov"
                                       , identification = Identification 9999999992}
                      , species = Cat}
                 ,Pet { name = "Bunny"
                      , owner = Person { firstName = "Petr"
                                       , lastName = "Ivanov"
                                       , identification = Identification 9999999992}
                      , species = Bunny}
                 ,Pet { name = "Thumper"
                      , owner = Person { firstName = "Petr"
                                       , lastName = "Ivanov"
                                       , identification = Identification 9999999992}
                      , species = Bunny}
                 ,Pet { name = "Peter"
                      , owner = Person { firstName = "Maria"
                                       , lastName = "Ivanova"
                                       , identification = Identification 1111111112}
                      , species = Dog}
                 ,Pet { name = "Roger"
                      , owner = Person { firstName = "Maria"
                                       , lastName = "Ivanova"
                                       , identification = Identification 1111111112}
                      , species = Cat}
                 ,Pet { name = "Demogorgon"
                      , owner = Person { firstName = "Masha"
                                       , lastName = "Ivanova"
                                       , identification = Identification 9999919999}
                      , species = Bunny}
                 ,Pet { name = "Fluffy"
                      , owner = Person { firstName = "Masha"
                                       , lastName = "Ivanova"
                                       , identification = Identification 9999919999}
                      , species = Tarantula}
                 ,Pet { name = "Inky"
                      , owner = Person { firstName = "Masha"
                                       , lastName = "Ivanova"
                                       , identification = Identification 9999919999}
                      , species = Tarantula}
                 ,Pet { name = "Apawllo"
                      , owner = Person { firstName = "Kate"
                                       , lastName = "Smith"
                                       , identification = Identification 2121212121}
                      , species = Cat}
                 ,Pet { name = "Catphrodite"
                      , owner = Person { firstName = "Kate"
                                       , lastName = "Smith"
                                       , identification = Identification 2121212121}
                      , species = Cat}
                 ,Pet { name = "Purrseidon"
                      , owner = Person { firstName = "Kate"
                                       , lastName = "Smith"
                                       , identification = Identification 2121212121}, species = Cat}
                 ,Pet { name = "Koshka"
                      , owner = Person { firstName = "Kate"
                                       , lastName = "Verbitskaia"
                                       , identification = Identification 1234567890}
                      , species = Cat}
                 ,Pet { name = "Dalai Pawma"
                      , owner = Person { firstName = "Kate"
                                       , lastName = "Verbitskaia"
                                       , identification = Identification 9876543210}
                      , species = Cat}
                 ,Pet { name = "Lucifur"
                      , owner = Person { firstName = "Kate"
                                       , lastName = "Verbitskaia"
                                       , identification = Identification 9876543210}
                      , species = Cat}
                 ,Pet { name = "Margaret Scratcher"
                      , owner = Person { firstName = "Kate"
                                       , lastName = "Verbitskaia"
                                       , identification = Identification 9876543210}
                      , species = Cat}
                 ,Pet { name = "Anthony Hopkins"
                      , owner = Person { firstName = "Maria"
                                       , lastName = "Verbitskaia"
                                       , identification = Identification 1111111111}
                      , species = Bunny}
                 ,Pet { name = "Bun-edict Cumberbatch"
                      , owner = Person { firstName = "Maria"
                                       , lastName = "Verbitskaia"
                                       , identification = Identification 1111111111}
                      , species = Bunny}
                 ,Pet { name = "David Hopperfield"
                      , owner = Person { firstName = "Maria"
                                       , lastName = "Verbitskaia"
                                       , identification = Identification 1111111111}
                      , species = Bunny}]


p1 :: Person
p1 =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , identification = Identification 1234567890 }

p2 :: Person
p2 =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , identification = Identification 9876543210 }

p3 :: Person
p3 =
  Person { firstName = "Kate"
         , lastName = "Smith"
         , identification = Identification 2121212121 }

p4 :: Person
p4 =
  Person { firstName = "Maria"
         , lastName = "Verbitskaia"
         , identification = Identification 1111111111 }

p5 :: Person
p5 =
  Person { firstName = "Maria"
         , lastName = "Ivanova"
         , identification = Identification 1111111112 }

p6 :: Person
p6 =
  Person { firstName = "Ivan"
         , lastName = "Ivanov"
         , identification = Identification 9999999999 }

p7 :: Person
p7 =
  Person { firstName = "Petr"
         , lastName = "Ivanov"
         , identification = Identification 9999999992 }

p8 :: Person
p8 =
  Person { firstName = "Masha"
         , lastName = "Ivanova"
         , identification = Identification 9999919999 }

pet8_1 =
  Pet { name = "Fluffy"
      , owner = p8
      , species = Tarantula
      }

pet8_2 =
  Pet { name = "Inky"
      , owner = p8
      , species = Tarantula
      }

pet8_3 =
  Pet { name = "Demogorgon"
      , owner = p8
      , species = Bunny
      }

pet7_1 =
  Pet { name = "Thumper"
      , owner = p7
      , species = Bunny
      }

pet7_2 =
  Pet { name = "Thumper"
      , owner = p7
      , species = Dog
      }

pet7_3 =
  Pet { name = "Thumper"
      , owner = p7
      , species = Cat
      }

pet7_4 =
  Pet { name = "Bunny"
      , owner = p7
      , species = Bunny
      }

pet6_1 =
  Pet { name = "Peter"
      , owner = p6
      , species = Dog
      }

pet6_2 =
  Pet { name = "Roger"
      , owner = p6
      , species = Cat
      }

pet6_3 =
  Pet { name = "Brer"
      , owner = p6
      , species = Bunny
      }

pet6_4 =
  Pet { name = "Bugs"
      , owner = p6
      , species = Tarantula
      }

pet5_1 =
  Pet { name = "Peter"
      , owner = p5
      , species = Dog
      }

pet5_2 =
  Pet { name = "Roger"
      , owner = p5
      , species = Cat
      }

pet4_1 =
  Pet { name = "Anthony Hopkins"
      , owner = p4
      , species = Bunny
      }

pet4_2 =
  Pet { name = "David Hopperfield"
      , owner = p4
      , species = Bunny
      }

pet4_3 =
  Pet { name = "Bun-edict Cumberbatch"
      , owner = p4
      , species = Bunny
      }

pet3_1 =
  Pet { name = "Apawllo"
      , owner = p3
      , species = Cat
      }

pet3_2 =
  Pet { name = "Purrseidon"
      , owner = p3
      , species = Cat
      }

pet3_3 =
  Pet { name = "Catphrodite"
      , owner = p3
      , species = Cat
      }


pet2_1 =
  Pet { name = "Dalai Pawma"
      , owner = p2
      , species = Cat
      }

pet2_2 =
  Pet { name = "Margaret Scratcher"
      , owner = p2
      , species = Cat
      }

pet2_3 =
  Pet { name = "Lucifur"
      , owner = p2
      , species = Cat
      }

pet1_1 =
  Pet { name = "Koshka"
      , owner = p1
      , species = Cat
      }
