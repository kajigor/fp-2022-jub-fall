module Test.Human where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Human
import ToString
import Human (IDCard(BirthCertificate, Passport))

human1 :: Human
human1 =
  Human { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = Passport (1234, 567890) }

human1Aged :: Human
human1Aged =
  Human { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 30
         , idNumber = Passport (1234, 567890) }

human1AgedTwice :: Human
human1AgedTwice =
  Human { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 31
         , idNumber = Passport (1234, 567890) }

human2 :: Human
human2 =
  Human { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42
         , idNumber = Passport (9876, 543210) }

human3 :: Human
human3 =
  Human { firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21
         , idNumber = Passport (2121, 212121) }

human4 :: Human
human4 =
  Human { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 23
         , idNumber = Passport (1111, 111111) }

human4NewLastName :: Human
human4NewLastName =
  Human { firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = ["Verbitskaia"]
         , age = 23
         , idNumber = Passport (1111, 111111) }

human4NewLastNameNewLastName :: Human
human4NewLastNameNewLastName =
  Human { firstName = "Maria"
         , lastName = "Sidorova"
         , formerLastNames = [ "Ivanova", "Verbitskaia" ]
         , age = 23
         , idNumber = Passport (1111, 111111) }

child1 :: Human
child1 =
  Human { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , idNumber = BirthCertificate ("0AB", 123456) }

child2 :: Human
child2 =
  Human { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = BirthCertificate ("1CD", 123456) }

notValid1 :: Human
notValid1 =
  Human { firstName = ""
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = Passport (1234, 567890) }

notValid2 :: Human
notValid2 =
  Human { firstName = "Kate"
         , lastName = ""
         , formerLastNames = []
         , age = 29
         , idNumber = Passport (1234, 567890) }

notValid3 :: Human
notValid3 =
  Human { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = -13
         , idNumber = BirthCertificate ("2EF", 123456) }

notValid4 :: Human
notValid4 =
  Human { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = Passport (1234, 567890) }


unit_ageUp = do
  ageUp human1 @?= human1Aged
  ageUp (ageUp human1) @?= human1AgedTwice

unit_updateLastName = do
  let human4' = updateLastName human4 "Ivanova"
  let human4'' = updateLastName human4NewLastName "Sidorova"
  human4' @?= human4NewLastName
  human4'' @?= human4NewLastNameNewLastName
  human4 @?= updateLastName human4 (lastName human4)


unit_namesakes = do
  assertBool "namesakes" (namesakes human1 human2)
  assertBool "notNamesakes: same human" (not $ namesakes human1 human1)
  assertBool "notNamesakes: same human" (not $ namesakes human1 human1Aged)
  assertBool "notNamesakes: different last name" (not $ namesakes human1 human3)
  assertBool "notNamesakes: different first name" (not $ namesakes human1 human4)

unit_toString = do
  toString human1 @?= "Kate Verbitskaia, 29"
  toString human1Aged @?= "Kate Verbitskaia, 30"
  toString human1AgedTwice @?= "Kate Verbitskaia, 31"
  toString human2 @?= "Kate Verbitskaia, 42"
  toString human3 @?= "Kate Smith, 21"
  toString human4 @?= "Maria Verbitskaia, 23"
  toString human4NewLastName @?= "Maria Ivanova, 23"
  toString human4NewLastNameNewLastName @?= "Maria Sidorova, 23"
  toString child1 @?= "Ivan Ivanov, 7"
  toString child2 @?= "Masha Ivanova, 3"

unit_valid = do
  assertBool "valid" (validateHuman human1)
  assertBool "valid" (validateHuman human1Aged)
  assertBool "valid" (validateHuman human1AgedTwice)
  assertBool "valid" (validateHuman human2)
  assertBool "valid" (validateHuman human3)
  assertBool "valid" (validateHuman human4)
  assertBool "valid" (validateHuman human4NewLastName)
  assertBool "valid" (validateHuman human4NewLastNameNewLastName)
  assertBool "valid" (validateHuman child1)
  assertBool "valid" (validateHuman child2)

  assertBool "not valid: no first name" (not $ validateHuman notValid1)
  assertBool "not valid: no last name" (not $ validateHuman notValid2)
  assertBool "not valid: negative age" (not $ validateHuman notValid3)
  assertBool "not valid: child with id" (not $ validateHuman notValid4)
