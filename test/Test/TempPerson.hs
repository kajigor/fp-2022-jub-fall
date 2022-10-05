module Test.TempPerson where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import TempPerson
import ToString
-- import TempPerson (IdType(Passport))
import TempPerson (TempPerson)


person1 :: TempPerson
person1 =
  TempPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idType = Passport (1234, 567890) }

person1Aged :: TempPerson
person1Aged =
  TempPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 30
         , idType = Passport (1234, 567890) }

person1AgedTwice :: TempPerson
person1AgedTwice =
  TempPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 31
         , idType = Passport (1234, 567890) }

person2 :: TempPerson
person2 =
  TempPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42
         , idType = Passport (9876, 543210) }

person3 :: TempPerson
person3 =
  TempPerson { firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21
         , idType = Passport (2121, 212121) }

person4 :: TempPerson
person4 =
  TempPerson { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 23
         , idType = Passport (1111, 111111) }

person4NewLastName :: TempPerson
person4NewLastName =
  TempPerson { firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = ["Verbitskaia"]
         , age = 23
         , idType = Passport (1111, 111111) }

person4NewLastNameNewLastName :: TempPerson
person4NewLastNameNewLastName =
  TempPerson { firstName = "Maria"
         , lastName = "Sidorova"
         , formerLastNames = [ "Ivanova", "Verbitskaia" ]
         , age = 23
         , idType = Passport (1111, 111111) }

child1 :: TempPerson
child1 =
  TempPerson { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , idType = Bc (0000, 000000) }

child2 :: TempPerson
child2 =
  TempPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idType = Bc (0000, 000000) }

notValid1 :: TempPerson
notValid1 =
  TempPerson { firstName = ""
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idType = Passport (1234, 567890) }

notValid2 :: TempPerson
notValid2 =
  TempPerson { firstName = "Kate"
         , lastName = ""
         , formerLastNames = []
         , age = 29
         , idType = Passport (1234, 567890) }

notValid3 :: TempPerson
notValid3 =
  TempPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = -13
         , idType = Passport (1234, 567890) }

notValid4 :: TempPerson
notValid4 =
  TempPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idType = Passport (1234, 567890) }


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




















