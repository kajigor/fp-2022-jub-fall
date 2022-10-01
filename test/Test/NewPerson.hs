module Test.NewPerson where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import NewPerson
import ToString ( ToString(toString) )

person1 :: NewPerson
person1 =
  NewPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idDocument = Passport 1234 567890 }

person1Aged :: NewPerson
person1Aged =
  NewPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 30
         , idDocument = Passport 1234 567890 }

person1AgedTwice :: NewPerson
person1AgedTwice =
  NewPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 31
         , idDocument = Passport 1234 567890 }

person2 :: NewPerson
person2 =
  NewPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42
         , idDocument = Passport 9876 543210 }

person3 :: NewPerson
person3 =
  NewPerson { firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21
         , idDocument = Passport 2121 212121 }

person4 :: NewPerson
person4 =
  NewPerson { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 23
         , idDocument = Passport 1111 111111 }

person4NewLastName :: NewPerson
person4NewLastName =
  NewPerson { firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = ["Verbitskaia"]
         , age = 23
         , idDocument = Passport 1111 111111 }

person4NewLastNameNewLastName :: NewPerson
person4NewLastNameNewLastName =
  NewPerson { firstName = "Maria"
         , lastName = "Sidorova"
         , formerLastNames = [ "Ivanova", "Verbitskaia" ]
         , age = 23
         , idDocument = Passport 1111 111111 }

child1 :: NewPerson
child1 =
  NewPerson { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , idDocument = BirthCertificate "IAB" 123456 }

child2 :: NewPerson
child2 =
  NewPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3 
         , idDocument = BirthCertificate "IICD" 456732 }

child3 :: NewPerson
child3 =
  NewPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 4
         , idDocument = BirthCertificate "VAY" 301917 }

notValid1 :: NewPerson
notValid1 =
  NewPerson { firstName = ""
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idDocument = Passport 1234 567890 }

notValid2 :: NewPerson
notValid2 =
  NewPerson { firstName = "Kate"
         , lastName = ""
         , formerLastNames = []
         , age = 29
         , idDocument = Passport 1234 567890 }

notValid3 :: NewPerson
notValid3 =
  NewPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = -13
         , idDocument = Passport 1234 567890 }

notValid4 :: NewPerson
notValid4 =
  NewPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idDocument = Passport 1234 567890 }


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
  assertBool "valid" (validateNewPerson person1)
  assertBool "valid" (validateNewPerson person1Aged)
  assertBool "valid" (validateNewPerson person1AgedTwice)
  assertBool "valid" (validateNewPerson person2)
  assertBool "valid" (validateNewPerson person3)
  assertBool "valid" (validateNewPerson person4)
  assertBool "valid" (validateNewPerson person4NewLastName)
  assertBool "valid" (validateNewPerson person4NewLastNameNewLastName)
  assertBool "valid" (validateNewPerson child1)
  assertBool "valid" (validateNewPerson child2)

  assertBool "not valid: no first name" (not $ validateNewPerson notValid1)
  assertBool "not valid: no last name" (not $ validateNewPerson notValid2)
  assertBool "not valid: negative age" (not $ validateNewPerson notValid3)
  assertBool "not valid: child with id" (not $ validateNewPerson notValid4)




















