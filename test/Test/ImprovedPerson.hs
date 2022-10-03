module Test.ImprovedPerson where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import ImprovedPerson
import ToString

person1 :: ImprovedPerson
person1 =
  ImprovedPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , certificateNumber = ("IVAB", 123456) }

person1Aged :: ImprovedPerson
person1Aged =
  ImprovedPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 30
         , certificateNumber = ("IVAB", 123456) }

person1AgedTwice :: ImprovedPerson
person1AgedTwice =
  ImprovedPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 31
         , certificateNumber = ("IVAB", 123456) }

person2 :: ImprovedPerson
person2 =
  ImprovedPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42
         , certificateNumber = ("VBD", 543210) }

person3 :: ImprovedPerson
person3 =
  ImprovedPerson { firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21
         , certificateNumber = ("IIIEE", 212121) }

person4 :: ImprovedPerson
person4 =
  ImprovedPerson { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 23
         , certificateNumber = ("IIAA", 111111) }

person4NewLastName :: ImprovedPerson
person4NewLastName =
  ImprovedPerson { firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = ["Verbitskaia"]
         , age = 23
         , certificateNumber = ("IIAA", 111111) }

person4NewLastNameNewLastName :: ImprovedPerson
person4NewLastNameNewLastName =
  ImprovedPerson { firstName = "Maria"
         , lastName = "Sidorova"
         , formerLastNames = [ "Ivanova", "Verbitskaia" ]
         , age = 23
         , certificateNumber = ("IIAA", 111111) }

child1 :: ImprovedPerson
child1 =
  ImprovedPerson { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , certificateNumber = ("IAA", 176884) }

child2 :: ImprovedPerson
child2 =
  ImprovedPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , certificateNumber = ("IIBB", 313373) }

notValid1 :: ImprovedPerson
notValid1 =
  ImprovedPerson { firstName = ""
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , certificateNumber = ("VCC", 567890) }

notValid2 :: ImprovedPerson
notValid2 =
  ImprovedPerson { firstName = "Kate"
         , lastName = ""
         , formerLastNames = []
         , age = 29
         , certificateNumber = ("IIICC", 567890) }

notValid3 :: ImprovedPerson
notValid3 =
  ImprovedPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = -13
         , certificateNumber = ("IBO", 567890) }

notValid4 :: ImprovedPerson
notValid4 =
  ImprovedPerson { firstName = "Elite"
         , lastName = "Troll"
         , formerLastNames = ["Emptystringish"]
         , age = 1337
         , certificateNumber = ("", 567890) }


newValid :: ImprovedPerson
newValid =
  ImprovedPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , certificateNumber = ("IRE", 567890) }


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
  assertBool "valid: child with id" (validatePerson newValid)

  assertBool "not valid: no first name" (not $ validatePerson notValid1)
  assertBool "not valid: no last name" (not $ validatePerson notValid2)
  assertBool "not valid: negative age" (not $ validatePerson notValid3)
  assertBool "not valid: empty certificateNumber series" (not $ validatePerson notValid4)




















