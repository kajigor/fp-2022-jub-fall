module Test.MyPerson where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import MyPerson
import ToString

person1 :: MyPerson
person1 =
  MyPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , document = Id { inumber = (1234, 567890) } 
  }

person1Aged :: MyPerson
person1Aged =
  MyPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 30
         , document = Id { inumber = (1234, 567890) } 
  }

person1AgedTwice :: MyPerson
person1AgedTwice =
  MyPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 31
         , document = Id { inumber = (1234, 567890) } }

person2 :: MyPerson
person2 =
  MyPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42
        , document = Id { inumber = (9876, 543210) } 
  }

person3 :: MyPerson
person3 =
  MyPerson { firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21
         , document = Id { inumber = (2121, 212121) } 
  }

person4 :: MyPerson
person4 =
  MyPerson { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 23
         , document = Id { inumber = (1111, 111111) } 
  }


person4NewLastName :: MyPerson
person4NewLastName =
  MyPerson { firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = ["Verbitskaia"]
         , age = 23
         , document = Id { inumber = (1111, 111111) } 
  }

person4NewLastNameNewLastName :: MyPerson
person4NewLastNameNewLastName =
  MyPerson { firstName = "Maria"
         , lastName = "Sidorova"
         , formerLastNames = [ "Ivanova", "Verbitskaia" ]
         , age = 23
         , document = Id { inumber = (1111, 111111) } 
  }

child1 :: MyPerson
child1 =
  MyPerson { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , document = BirthCert { bnumber = 54 } 
  }

child2 :: MyPerson
child2 =
  MyPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , document = BirthCert { bnumber = 124 } 
  }

notValid1 :: MyPerson
notValid1 =
  MyPerson { firstName = ""
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , document = Id { inumber = (1234, 567890) } 
  }

notValid2 :: MyPerson
notValid2 =
  MyPerson { firstName = "Kate"
         , lastName = ""
         , formerLastNames = []
         , age = 29
         , document = Id { inumber = (1234, 567890) } 
  }

notValid3 :: MyPerson
notValid3 =
  MyPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = -13
         , document = Id { inumber = (1234, 567890) } 
  }

notValid4 :: MyPerson
notValid4 =
  MyPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , document = Id { inumber = (1234, 567890) } 
  }

notValid5 :: MyPerson
notValid5 =
  MyPerson { firstName = "Masha"
         , lastName = "Ivanchura"
         , formerLastNames = []
         , age = 19
         , document = BirthCert { bnumber = 567890 } 
  }

unit_ageUp = do
  ageUp person1 @?= person1Aged
  ageUp (ageUp person1) @?= person1AgedTwice

unit_updateLastName = do
  let person4' = updateLastName person4 "Ivanova"
  let person4'' = updateLastName person4NewLastName "Sidorova"
  person4' @?= person4NewLastName
  person4'' @?= person4NewLastNameNewLastName
  person4 @?= updateLastName person4 (lastName person4)


unit_namesakes = do
  assertBool "namesakes" (namesakes person1 person2)
  assertBool "notNamesakes: same person" (not $ namesakes person1 person1)
  assertBool "notNamesakes: same person" (not $ namesakes person1 person1Aged)
  assertBool "notNamesakes: different last name" (not $ namesakes person1 person3)
  assertBool "notNamesakes: different first name" (not $ namesakes person1 person4)

unit_toString = do
  toString person1 @?= "Kate Verbitskaia, 29"
  toString person1Aged @?= "Kate Verbitskaia, 30"
  toString person1AgedTwice @?= "Kate Verbitskaia, 31"
  toString person2 @?= "Kate Verbitskaia, 42"
  toString person3 @?= "Kate Smith, 21"
  toString person4 @?= "Maria Verbitskaia, 23"
  toString person4NewLastName @?= "Maria Ivanova, 23"
  toString person4NewLastNameNewLastName @?= "Maria Sidorova, 23"
  toString child1 @?= "Ivan Ivanov, 7"
  toString child2 @?= "Masha Ivanova, 3"

unit_valid = do
  assertBool "valid" (validateMyPerson person1)
  assertBool "valid" (validateMyPerson person1Aged)
  assertBool "valid" (validateMyPerson person1AgedTwice)
  assertBool "valid" (validateMyPerson person2)
  assertBool "valid" (validateMyPerson person3)
  assertBool "valid" (validateMyPerson person4)
  assertBool "valid" (validateMyPerson person4NewLastName)
  assertBool "valid" (validateMyPerson person4NewLastNameNewLastName)
  assertBool "valid" (validateMyPerson child1)
  assertBool "valid" (validateMyPerson child2)

  assertBool "not valid: no first name" (not $ validateMyPerson notValid1)
  assertBool "not valid: no last name" (not $ validateMyPerson notValid2)
  assertBool "not valid: negative age" (not $ validateMyPerson notValid3)
  assertBool "not valid: child with id" (not $ validateMyPerson notValid4)
  assertBool "not valid: adult with birthday certificate" (not $ validateMyPerson notValid5)
