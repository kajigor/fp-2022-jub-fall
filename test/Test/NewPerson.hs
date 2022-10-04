module Test.NewPerson where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import NewPerson
import ToString

person1 :: NewPerson
person1 =
  NewPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = (1234, 567890)
         , birthCertificate = (BirthCertificate 1 "ZA" 915949)  }

person1Aged :: NewPerson
person1Aged =
  NewPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 30
         , idNumber = (1234, 567890)
         , birthCertificate = (BirthCertificate 1 "ZA" 915949) }

person1AgedTwice :: NewPerson
person1AgedTwice =
  NewPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 31
         , idNumber = (1234, 567890)
         , birthCertificate = (BirthCertificate 1 "ZA" 915949) }

person2 :: NewPerson
person2 =
  NewPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42
         , idNumber = (9876, 543210)
         , birthCertificate = (BirthCertificate 8 "SW" 100000) }

person3 :: NewPerson
person3 =
  NewPerson { firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21
         , idNumber = (2121, 212121)
         , birthCertificate = (BirthCertificate 3 "AH" 527844) }

person4 :: NewPerson
person4 =
  NewPerson { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 23
         , idNumber = (1111, 111111)
         , birthCertificate = (BirthCertificate 8 "SW" 129003) }

person4NewLastName :: NewPerson
person4NewLastName =
  NewPerson { firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = ["Verbitskaia"]
         , age = 23
         , idNumber = (1111, 111111)
         , birthCertificate = (BirthCertificate 8 "SW" 129003) }

person4NewLastNameNewLastName :: NewPerson
person4NewLastNameNewLastName =
  NewPerson { firstName = "Maria"
         , lastName = "Sidorova"
         , formerLastNames = [ "Ivanova", "Verbitskaia" ]
         , age = 23
         , idNumber = (1111, 111111)
         , birthCertificate = (BirthCertificate 8 "SW" 129003) }

child1 :: NewPerson
child1 =
  NewPerson { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , idNumber = (0000, 000000)
         , birthCertificate = (BirthCertificate 9 "OO" 256432) }

child2 :: NewPerson
child2 =
  NewPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = (0000, 000000)
         , birthCertificate = (BirthCertificate 0 "QA" 346248) }

child3 :: NewPerson
child3 =
  NewPerson { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , idNumber = (0000, 000000)
         , birthCertificate = (BirthCertificate 3 "MN" 883788) }

notValid1 :: NewPerson
notValid1 =
  NewPerson { firstName = ""
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , idNumber = (1234, 567890)
         , birthCertificate = (BirthCertificate 1 "XX" 999999) }

notValid2 :: NewPerson
notValid2 =
  NewPerson { firstName = "Kate"
         , lastName = ""
         , formerLastNames = []
         , age = 29
         , idNumber = (1234, 567890)
         , birthCertificate = (BirthCertificate 1 "XX" 999999) }

notValid3 :: NewPerson
notValid3 =
  NewPerson { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = -13
         , idNumber = (1234, 567890)
         , birthCertificate = (BirthCertificate 1 "XX" 999999) }

notValid4 :: NewPerson
notValid4 =
  NewPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = (1234, 567890)
         , birthCertificate = (BirthCertificate 1 "XX" 999999) }

notValid5 :: NewPerson
notValid5 =
  NewPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , idNumber = (0000, 000000)
         , birthCertificate = (BirthCertificate 1 "XXO" 999999) }

notValid6 :: NewPerson
notValid6 =
  NewPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 23
         , idNumber = (1234, 567890)
         , birthCertificate = (BirthCertificate 1 "" 999999) }

notValid7 :: NewPerson
notValid7 =
  NewPerson { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 23
         , idNumber = (1234, 567890)
         , birthCertificate = (BirthCertificate 1 "aa" 100000) }

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
  assertBool "namesakes" (namesakes child1 child3)

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
  assertBool "not valid: birth certificate 3 letters" (not $ validatePerson notValid5)
  assertBool "not valid: birth certificate 0 letters" (not $ validatePerson notValid6)
  assertBool "not valid: birth certificate wrong letters" (not $ validatePerson notValid7)



















