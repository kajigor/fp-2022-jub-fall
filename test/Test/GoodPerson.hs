module Test.GoodPerson where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import GoodPerson
import ToString


gperson1 :: GoodPerson
gperson1 =
  GoodPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = (1234, 567890)
         , birthCertificate = ("HT", 185734)}

gperson2 :: GoodPerson
gperson2 =
  GoodPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 13
         , idNumber = (0, 0)
         , birthCertificate = ("HT", 534677)}

badperson :: GoodPerson
badperson =
  GoodPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 13
         , idNumber = (1234, 553645)
         , birthCertificate = ("HTsdfasdfasdf", 534677)}




person1 :: GoodPerson
person1 =
  GoodPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = (1234, 567890)
         , birthCertificate = ("cy", 453664)}

person1Aged :: GoodPerson
person1Aged =
  GoodPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 30
         , idNumber = (1234, 567890)
         , birthCertificate = ("cy", 453664)}

person1AgedTwice :: GoodPerson
person1AgedTwice =
  GoodPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 31
         , idNumber = (1234, 567890)
         , birthCertificate = ("cy", 453664)}

person2 :: GoodPerson
person2 =
  GoodPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42
         , idNumber = (9876, 543210)
         , birthCertificate = ("je", 453654)}

person3 :: GoodPerson
person3 =
  GoodPerson { firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21
         , idNumber = (2121, 212121)
         , birthCertificate = ("fi", 453664)}

person4 :: GoodPerson
person4 =
  GoodPerson { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 23
         , idNumber = (1111, 111111)
         , birthCertificate = ("rt", 453264)}

person4NewLastName :: GoodPerson
person4NewLastName =
  GoodPerson { firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = ["Verbitskaia"]
         , age = 23
         , idNumber = (1111, 111111)
         , birthCertificate = ("rt", 453264)}

person4NewLastNameNewLastName :: GoodPerson
person4NewLastNameNewLastName =
  GoodPerson { firstName = "Maria"
         , lastName = "Sidorova"
         , formerLastNames = [ "Ivanova", "Verbitskaia" ]
         , age = 23
         , idNumber = (1111, 111111)
         , birthCertificate = ("rt", 453264)}

child1 :: GoodPerson
child1 =
  GoodPerson { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , idNumber = (0000, 000000)
         , birthCertificate = ("jw", 453464)}

child2 :: GoodPerson
child2 =
  GoodPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = (0000, 000000)
         , birthCertificate = ("sy", 453361)}

notValid1 :: GoodPerson
notValid1 =
  GoodPerson { firstName = ""
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = (1234, 567890)
         , birthCertificate = ("hw", 457464)}

notValid2 :: GoodPerson
notValid2 =
  GoodPerson { firstName = "Kate"
         , lastName = ""
         , formerLastNames = []
         , age = 29
         , idNumber = (1234, 567890)
         , birthCertificate = ("st", 453534)}

notValid3 :: GoodPerson
notValid3 =
  GoodPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = -13
         , idNumber = (1234, 567890)
         , birthCertificate = ("ge", 453564)}

notValid4 :: GoodPerson
notValid4 =
  GoodPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = (1234, 567890)
         , birthCertificate = ("ty", 453534)}


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
  assertBool "valid" (validatePerson gperson1)
  assertBool "valid" (validatePerson gperson2)


  assertBool "not valid: no first name" (not $ validatePerson notValid1)
  assertBool "not valid: no last name" (not $ validatePerson notValid2)
  assertBool "not valid: negative age" (not $ validatePerson notValid3)
  assertBool "not valid: child with id" (not $ validatePerson notValid4)
  assertBool "not valid: child with id" (not $ validatePerson badperson)




















