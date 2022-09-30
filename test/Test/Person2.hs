module Test.Person2 where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Person2
import ToString

person1 :: Person2
person1 =
  Person2 { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = PassportNumber 1234 567890 }

person1Aged :: Person2
person1Aged =
  Person2 { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 30
         , idNumber = PassportNumber 1234 567890 }

person1AgedTwice :: Person2
person1AgedTwice =
  Person2 { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 31
         , idNumber = PassportNumber 1234 567890 }

person2 :: Person2
person2 =
  Person2 { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42
         , idNumber = PassportNumber 9876 543210 }

person3 :: Person2
person3 =
  Person2 { firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21
         , idNumber = PassportNumber 2121 212121 }

person4 :: Person2
person4 =
  Person2 { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 23
         , idNumber = PassportNumber 1111 111111 }

person4NewLastName :: Person2
person4NewLastName =
  Person2 { firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = ["Verbitskaia"]
         , age = 23
         , idNumber = PassportNumber 1111 111111 }

person4NewLastNameNewLastName :: Person2
person4NewLastNameNewLastName =
  Person2 { firstName = "Maria"
         , lastName = "Sidorova"
         , formerLastNames = [ "Ivanova", "Verbitskaia" ]
         , age = 23
         , idNumber = PassportNumber 1111 111111 }

child1 :: Person2
child1 =
  Person2 { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , idNumber = BirthCertificateNumber 1234 }

child2 :: Person2
child2 =
  Person2 { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = BirthCertificateNumber 2345 }

notValid1 :: Person2
notValid1 =
  Person2 { firstName = ""
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = PassportNumber 1111 111111 }

notValid2 :: Person2
notValid2 =
  Person2 { firstName = "Kate"
         , lastName = ""
         , formerLastNames = []
         , age = 29
         , idNumber = PassportNumber 1111 111111 }

notValid3 :: Person2
notValid3 =
  Person2 { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = -13
         , idNumber = PassportNumber 1111 111111 }

notValid4 :: Person2
notValid4 =
  Person2 { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = PassportNumber 1111 111111 }

notValid5 :: Person2
notValid5 =
  Person2 { firstName = "Masha"
          , lastName = "Ivanova"
          , formerLastNames = []
          , age = 25
          , idNumber = BirthCertificateNumber 2345 } 

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
  assertBool "valid" (validatePerson person1)
  assertBool "valid" (validatePerson person1Aged)
  assertBool "valid" (validatePerson person1AgedTwice)
  assertBool "valid" (validatePerson person2)
  assertBool "valid" (validatePerson person3)
  assertBool "valid" (validatePerson person4)
  assertBool "valid" (validatePerson person4NewLastName)
  assertBool "valid" (validatePerson person4NewLastNameNewLastName)
  assertBool "valid" (validatePerson child1)
  assertBool "valid" (validatePerson child2)

  assertBool "not valid: no first name" (not $ validatePerson notValid1)
  assertBool "not valid: no last name" (not $ validatePerson notValid2)
  assertBool "not valid: negative age" (not $ validatePerson notValid3)
  assertBool "not valid: child with id" (not $ validatePerson notValid4)
  assertBool "not valid: adult with birth cert" (not $ validatePerson notValid5)
