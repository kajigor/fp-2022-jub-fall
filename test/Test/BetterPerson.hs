module Test.BetterPerson where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import BetterPerson
import ToString

person1 :: Person
person1 =
  Adult { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , passport = (1234, 567890) }

person1Aged :: Person
person1Aged =
  Adult { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 30
         , passport = (1234, 567890) }

person1AgedTwice :: Person
person1AgedTwice =
  Adult { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 31
         , passport = (1234, 567890) }

person2 :: Person
person2 =
  Adult { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42
         , passport = (9876, 543210) }

person3 :: Person
person3 =
  Adult { firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21
         , passport = (2121, 212121) }

person4 :: Person
person4 =
  Adult { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 23
         , passport = (1111, 111111) }

person4NewLastName :: Person
person4NewLastName =
  Adult { firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = ["Verbitskaia"]
         , age = 23
         , passport = (1111, 111111) }

person4NewLastNameNewLastName :: Person
person4NewLastNameNewLastName =
  Adult { firstName = "Maria"
         , lastName = "Sidorova"
         , formerLastNames = [ "Ivanova", "Verbitskaia" ]
         , age = 23
         , passport = (1111, 111111) }

person5 :: Person
person5 =
  Adult { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 17
         , passport = (1234, 567890) }

child1 :: Person
child1 =
  Child { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , birthCertificate = (1234, 567890) }

child2 :: Person
child2 =
  Child { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , birthCertificate = (1234, 567890) }

child3 :: Person
child3 =
  Child { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , birthCertificate = (1234, 567891) }

notValid1 :: Person
notValid1 =
  Adult { firstName = ""
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , passport = (1234, 567890) }

notValid2 :: Person
notValid2 =
  Adult { firstName = "Kate"
         , lastName = ""
         , formerLastNames = []
         , age = 29
         , passport = (1234, 567890) }

notValid3 :: Person
notValid3 =
  Child { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = -13
         , birthCertificate = (1234, 567890) }


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
  assertBool "namesakes: person and child" (namesakes person5 child1)
  assertBool "namesakes: child and child" (namesakes child1 child3)
  assertBool "notNamesakes: same person" (not $ namesakes child1 child1)

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
