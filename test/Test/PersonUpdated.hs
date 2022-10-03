module Test.PersonUpdated where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import PersonUpdated
import ToString

person1 :: PersonUpdated
person1 =
  PersonUpdated { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = (1234, 567890)
         , birthCertificate = ("V12", 845690) }

person1Aged :: PersonUpdated
person1Aged =
  PersonUpdated { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 30
         , idNumber = (1234, 567890)
         , birthCertificate = ("V12", 845690) }

person1AgedTwice :: PersonUpdated
person1AgedTwice =
  PersonUpdated { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 31
         , idNumber = (1234, 567890)
         , birthCertificate = ("V12", 845690) }

person2 :: PersonUpdated
person2 =
  PersonUpdated { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42
         , idNumber = (9876, 543210)
         , birthCertificate = ("V01", 010305) }

person3 :: PersonUpdated
person3 =
  PersonUpdated { firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21
         , idNumber = (2121, 212121)
         , birthCertificate = ("V02", 122365) }

person4 :: PersonUpdated
person4 =
  PersonUpdated { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 23
         , idNumber = (1111, 111111)
         , birthCertificate = ("V30", 911527) }

person4NewLastName :: PersonUpdated
person4NewLastName =
  PersonUpdated { firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = ["Verbitskaia"]
         , age = 23
         , idNumber = (1111, 111111)
         , birthCertificate = ("V30", 911527) }

person4NewLastNameNewLastName :: PersonUpdated
person4NewLastNameNewLastName =
  PersonUpdated { firstName = "Maria"
         , lastName = "Sidorova"
         , formerLastNames = [ "Ivanova", "Verbitskaia" ]
         , age = 23
         , idNumber = (1111, 111111)
         , birthCertificate = ("V30", 911527) }

child1 :: PersonUpdated
child1 =
  PersonUpdated { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , idNumber = (0000, 000000)
         , birthCertificate = ("V17", 423170) }

child2 :: PersonUpdated
child2 =
  PersonUpdated { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = (0000, 000000)
         , birthCertificate = ("V20", 651801) }

child3 :: PersonUpdated
child3 =
  PersonUpdated { firstName = "Emily"
         , lastName = "Alfred"
         , formerLastNames = []
         , age = 9
         , idNumber = (0000, 000000)
         , birthCertificate = ("V27", 401846) }

notValid1 :: PersonUpdated
notValid1 =
  PersonUpdated { firstName = ""
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = (1234, 567890)
         , birthCertificate = ("V51", 472189) }

notValid2 :: PersonUpdated
notValid2 =
  PersonUpdated { firstName = "Kate"
         , lastName = ""
         , formerLastNames = []
         , age = 29
         , idNumber = (1234, 567890)
         , birthCertificate = ("V19", 914914) }

notValid3 :: PersonUpdated
notValid3 =
  PersonUpdated { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = -13
         , idNumber = (1234, 567890) }

notValid4 :: PersonUpdated
notValid4 =
  PersonUpdated { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = (1234, 567890)
         , birthCertificate = ("V05", 011037) }


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
  assertBool "namesakes: different birth certificates" (namesakes person1 person2)
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
  toString child3 @?= "Emily Alfred, 9"

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
  assertBool "valid" (validatePerson child3)

  assertBool "not valid: no first name" (not $ validatePerson notValid1)
  assertBool "not valid: no last name" (not $ validatePerson notValid2)
  assertBool "not valid: negative age" (not $ validatePerson notValid3)
  assertBool "not valid: child with idNumber" (not $ validatePerson notValid4)
