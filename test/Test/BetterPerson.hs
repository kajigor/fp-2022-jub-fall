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
                 , idNumber = (1234, 567890)
                 , birthCert = ("sf", 123456) }

person1Aged :: BetterPerson
person1Aged =
    BetterPerson { firstName = "Kate"
                 , lastName = "Verbitskaia"
                 , formerLastNames = []
                 , age = 30
                 , idNumber = (1234, 567890)
                 , birthCert = ("sf", 123456)}

person1AgedTwice :: BetterPerson
person1AgedTwice =
    BetterPerson { firstName = "Kate"
                 , lastName = "Verbitskaia"
                 , formerLastNames = []
                 , age = 31
                 , idNumber = (1234, 567890)
                 , birthCert = ("sf", 123456)}

person2 :: BetterPerson
person2 =
    BetterPerson { firstName = "Kate"
                 , lastName = "Verbitskaia"
                 , formerLastNames = []
                 , age = 42
                 , idNumber = (9876, 543210)
                 , birthCert = ("sa", 123456)}

person3 :: BetterPerson
person3 =
    BetterPerson { firstName = "Kate"
                 , lastName = "Smith"
                 , formerLastNames = []
                 , age = 21
                 , idNumber = (2121, 212121)
                 , birthCert = ("pw", 134456)}

person4 :: BetterPerson
person4 =
    BetterPerson { firstName = "Maria"
                 , lastName = "Verbitskaia"
                 , formerLastNames = []
                 , age = 23
                 , idNumber = (1111, 111111)
                 , birthCert = ("pa", 987654)}

person4NewLastName :: BetterPerson
person4NewLastName =
    BetterPerson { firstName = "Maria"
                 , lastName = "Ivanova"
                 , formerLastNames = ["Verbitskaia"]
                 , age = 23
                 , idNumber = (1111, 111111)
                 , birthCert = ("pa", 987654) }

person4NewLastNameNewLastName :: BetterPerson
person4NewLastNameNewLastName =
    BetterPerson { firstName = "Maria"
                 , lastName = "Sidorova"
                 , formerLastNames = [ "Ivanova", "Verbitskaia" ]
                 , age = 23
                 , idNumber = (1111, 111111)
                 , birthCert = ("pa", 987654) }

child1 :: BetterPerson
child1 =
    BetterPerson { firstName = "Ivan"
                 , lastName = "Ivanov"
                 , formerLastNames = []
                 , age = 7
                 , idNumber = (0000, 000000)
                 , birthCert = ("us", 456789) }

child2 :: BetterPerson
child2 =
    BetterPerson { firstName = "Masha"
                 , lastName = "Ivanova"
                 , formerLastNames = []
                 , age = 3
                 , idNumber = (0000, 000000)
                 , birthCert = ("pa", 102938) }

notValid1 :: BetterPerson
notValid1 =
    BetterPerson { firstName = ""
                 , lastName = "Verbitskaia"
                 , formerLastNames = []
                 , age = 29
                 , idNumber = (1234, 567890)
                 , birthCert = ("rs", 475834) }

notValid2 :: BetterPerson
notValid2 =
    BetterPerson { firstName = "Kate"
                 , lastName = ""
                 , formerLastNames = []
                 , age = 29
                 , idNumber = (1234, 567890)
                 , birthCert = ("au", 444666) }

notValid3 :: BetterPerson
notValid3 =
    BetterPerson { firstName = "Kate"
                 , lastName = "Verbitskaia"
                 , formerLastNames = []
                 , age = -13
                 , idNumber = (1234, 567890)
                 , birthCert = ("oa", 727272) }

notValid4 :: BetterPerson
notValid4 =
    BetterPerson { firstName = "Masha"
                 , lastName = "Ivanova"
                 , formerLastNames = []
                 , age = 3
                 , idNumber = (1234, 567890)
                 , birthCert = ("yy", 123456) }


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
