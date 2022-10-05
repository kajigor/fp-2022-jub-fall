module Test.BetterPerson where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import BetterPerson
import ToString

person1 :: BetterPerson
person1 =
  BetterPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = (PassportNumber 1234 567890) }

person1Aged :: BetterPerson
person1Aged =
  BetterPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 30
         , idNumber = (PassportNumber 1234 567890) }

person1AgedTwice :: BetterPerson
person1AgedTwice =
  BetterPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 31
         , idNumber = (PassportNumber 1234 567890) }

person2 :: BetterPerson
person2 =
  BetterPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42
         , idNumber = (PassportNumber 9876 543210) }

person3 :: BetterPerson
person3 =
  BetterPerson { firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21
         , idNumber = (PassportNumber 2121 212121) }

person4 :: BetterPerson
person4 =
  BetterPerson { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 23
         , idNumber = (PassportNumber 1111 111111) }

person4NewLastName :: BetterPerson
person4NewLastName =
  BetterPerson { firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = ["Verbitskaia"]
         , age = 23
         , idNumber = (PassportNumber 1111 111111) }

person4NewLastNameNewLastName :: BetterPerson
person4NewLastNameNewLastName =
  BetterPerson { firstName = "Maria"
         , lastName = "Sidorova"
         , formerLastNames = [ "Ivanova", "Verbitskaia" ]
         , age = 23
         , idNumber = (PassportNumber 1111 111111) }

child1 :: BetterPerson
child1 =
  BetterPerson { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , idNumber = (BirthCertificate "I-HS" 123456) }

child1Aged :: BetterPerson
child1Aged =
  BetterPerson { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 8
         , idNumber = (BirthCertificate "I-HS" 123456) }

child2 :: BetterPerson
child2 =
  BetterPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = (BirthCertificate "III-ML" 123456) }

notValid1 :: BetterPerson
notValid1 =
  BetterPerson { firstName = ""
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = (PassportNumber 1234 567890) }

notValid2 :: BetterPerson
notValid2 =
  BetterPerson { firstName = "Kate"
         , lastName = ""
         , formerLastNames = []
         , age = 29
         , idNumber = (PassportNumber 1234 567890) }

notValid3 :: BetterPerson
notValid3 =
  BetterPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = -13
         , idNumber = (PassportNumber 1234 567890) }

notValid4 :: BetterPerson
notValid4 =
  BetterPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = (PassportNumber 1234 567890) }

notValid5 :: BetterPerson
notValid5 =
  BetterPerson { firstName = "Gena"
         , lastName = "Krokodil"
         , formerLastNames = []
         , age = 20
         , idNumber = (BirthCertificate "IX-OR" 123456) }


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
  assertBool "notNamesakes: same child" (not $ namesakes child1 child1Aged)
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
  assertBool "valid" (validateBetterPerson person1)
  assertBool "valid" (validateBetterPerson person1Aged)
  assertBool "valid" (validateBetterPerson person1AgedTwice)
  assertBool "valid" (validateBetterPerson person2)
  assertBool "valid" (validateBetterPerson person3)
  assertBool "valid" (validateBetterPerson person4)
  assertBool "valid" (validateBetterPerson person4NewLastName)
  assertBool "valid" (validateBetterPerson person4NewLastNameNewLastName)
  assertBool "valid" (validateBetterPerson child1)
  assertBool "valid" (validateBetterPerson child2)

  assertBool "not valid: no first name" (not $ validateBetterPerson notValid1)
  assertBool "not valid: no last name" (not $ validateBetterPerson notValid2)
  assertBool "not valid: negative age" (not $ validateBetterPerson notValid3)
  assertBool "not valid: child with passport" (not $ validateBetterPerson notValid4)
  assertBool "not valid: adult with birth certificate" (not $ validateBetterPerson notValid5)




















