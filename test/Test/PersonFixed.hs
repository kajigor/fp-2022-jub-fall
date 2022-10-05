module Test.PersonFixed where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import PersonFixed
import ToString

person1 :: PersonFixed
person1 =
  PersonFixed { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = PassportNo (1234, 567890) }

person1Aged :: PersonFixed
person1Aged =
  PersonFixed { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 30
         , idNumber = PassportNo (1234, 567890) }

person1AgedTwice :: PersonFixed
person1AgedTwice =
  PersonFixed { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 31
         , idNumber = PassportNo (1234, 567890) }

person2 :: PersonFixed
person2 =
  PersonFixed { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42
         , idNumber = PassportNo (9876, 543210) }

person3 :: PersonFixed
person3 =
  PersonFixed { firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21
         , idNumber = PassportNo (2121, 212121) }

person4 :: PersonFixed
person4 =
  PersonFixed { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 23
         , idNumber = PassportNo (1111, 111111) }

person4NewLastName :: PersonFixed
person4NewLastName =
  PersonFixed { firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = ["Verbitskaia"]
         , age = 23
         , idNumber = PassportNo (1111, 111111) }

person4NewLastNameNewLastName :: PersonFixed
person4NewLastNameNewLastName =
  PersonFixed { firstName = "Maria"
         , lastName = "Sidorova"
         , formerLastNames = [ "Ivanova", "Verbitskaia" ]
         , age = 23
         , idNumber = PassportNo (1111, 111111) }

child1 :: PersonFixed
child1 =
  PersonFixed { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , idNumber = BirthCertificateNo (0000, 000000) }

child2 :: PersonFixed
child2 =
  PersonFixed { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = BirthCertificateNo (0000, 000000) }

notValid1 :: PersonFixed
notValid1 =
  PersonFixed { firstName = ""
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = PassportNo (1234, 567890) }

notValid2 :: PersonFixed
notValid2 =
  PersonFixed { firstName = "Kate"
         , lastName = ""
         , formerLastNames = []
         , age = 29
         , idNumber = PassportNo (1234, 567890) }

notValid3 :: PersonFixed
notValid3 =
  PersonFixed { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = -13
         , idNumber = PassportNo (1234, 567890) }

notValid4 :: PersonFixed
notValid4 =
  PersonFixed { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = PassportNo (1234, 567890) }


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




















