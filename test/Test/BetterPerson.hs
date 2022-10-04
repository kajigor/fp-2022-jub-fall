module Test.BetterPerson where

import           BetterPerson     (BetterPerson (..), PersonalId (..), ageUp,
                                   namesakes, updateLastName, validatePerson)
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           ToString

person1 :: BetterPerson
person1 =
  BetterPerson
    { firstName = "Kate"
    , lastName = "Verbitskaia"
    , formerLastNames = []
    , age = 29
    , personalId = Passport (1234, 567890) ("XV", "АБ", 123456)
    }

person1Aged :: BetterPerson
person1Aged =
  BetterPerson
    { firstName = "Kate"
    , lastName = "Verbitskaia"
    , formerLastNames = []
    , age = 30
    , personalId = Passport (1234, 567890) ("XV", "АБ", 123456)
    }

person1AgedTwice :: BetterPerson
person1AgedTwice =
  BetterPerson
    { firstName = "Kate"
    , lastName = "Verbitskaia"
    , formerLastNames = []
    , age = 31
    , personalId = Passport (1234, 567890) ("XV", "АБ", 123456)
    }

person2 :: BetterPerson
person2 =
  BetterPerson
    { firstName = "Kate"
    , lastName = "Verbitskaia"
    , formerLastNames = []
    , age = 42
    , personalId = Passport (9876, 543210) ("XV", "АБ", 123456)
    }

person3 :: BetterPerson
person3 =
  BetterPerson
    { firstName = "Kate"
    , lastName = "Smith"
    , formerLastNames = []
    , age = 21
    , personalId = Passport (2121, 212121) ("XV", "АБ", 123456)
    }

person4 :: BetterPerson
person4 =
  BetterPerson
    { firstName = "Maria"
    , lastName = "Verbitskaia"
    , formerLastNames = []
    , age = 23
    , personalId = Passport (1111, 111111) ("XV", "АБ", 123456)
    }

person4NewLastName :: BetterPerson
person4NewLastName =
  BetterPerson
    { firstName = "Maria"
    , lastName = "Ivanova"
    , formerLastNames = ["Verbitskaia"]
    , age = 23
    , personalId = Passport (1111, 111111) ("XV", "АБ", 123456)
    }

person4NewLastNameNewLastName :: BetterPerson
person4NewLastNameNewLastName =
  BetterPerson
    { firstName = "Maria"
    , lastName = "Sidorova"
    , formerLastNames = ["Ivanova", "Verbitskaia"]
    , age = 23
    , personalId = Passport (1111, 111111) ("XV", "АБ", 123456)
    }

child1 :: BetterPerson
child1 =
  BetterPerson
    { firstName = "Ivan"
    , lastName = "Ivanov"
    , formerLastNames = []
    , age = 7
    , personalId = Passport (0000, 000000) ("XV", "АБ", 123456) -- should not be valid due to having passport
    }

child2 :: BetterPerson
child2 =
  BetterPerson
    { firstName = "Masha"
    , lastName = "Ivanova"
    , formerLastNames = []
    , age = 3
    , personalId = BirthCertificate ("ab", "АБ", 123456) -- should not be valid because `ab` is not in ['X', 'V', 'I']
    }

child3 :: BetterPerson
child3 =
  BetterPerson
    { firstName = "Masha"
    , lastName = "Ivanova"
    , formerLastNames = []
    , age = 3
    , personalId = BirthCertificate ("XV", "ab", 123456) -- should not be valid because `ab` is not cyrillic
    }

child4 :: BetterPerson
child4 =
  BetterPerson
    { firstName = "Masha"
    , lastName = "Ivanova"
    , formerLastNames = []
    , age = 3
    , personalId = BirthCertificate ("XV", "АБВ", 123456) -- should not be valid because `АБВ` length != 2
    }

child5 :: BetterPerson
child5 =
  BetterPerson
    { firstName = "Masha"
    , lastName = "Ivanova"
    , formerLastNames = []
    , age = 3
    , personalId = BirthCertificate ("XV", "АБВ", 1234567) -- should not be valid because `1234567` length != 6
    }

validChild1 :: BetterPerson
validChild1 =
  BetterPerson
    { firstName = "Masha"
    , lastName = "Ivanova"
    , formerLastNames = []
    , age = 3
    , personalId = BirthCertificate ("XV", "АБ", 123456)
    }

validChild2 :: BetterPerson
validChild2 =
  BetterPerson
    { firstName = "Masha"
    , lastName = "Ivanova"
    , formerLastNames = []
    , age = 10
    , personalId = BirthCertificate ("XV", "АБ", 123456)
    }

validChild3 :: BetterPerson
validChild3 =
  BetterPerson
    { firstName = "Masha"
    , lastName = "Ivanova"
    , formerLastNames = []
    , age = 3
    , personalId = BirthCertificate ("XV", "АБ", 654321)
    }
  
validChild3WithPassport :: BetterPerson
validChild3WithPassport =
  BetterPerson
    { firstName = "Masha"
    , lastName = "Ivanova"
    , formerLastNames = []
    , age = 25
    , personalId = Passport (2121, 212121) ("XV", "АБ", 654321)
    }

notValid1 :: BetterPerson
notValid1 =
  BetterPerson
    { firstName = ""
    , lastName = "Verbitskaia"
    , formerLastNames = []
    , age = 29
    , personalId = Passport (1234, 567890) ("XV", "АБ", 123456)
    }

notValid2 :: BetterPerson
notValid2 =
  BetterPerson
    { firstName = "Kate"
    , lastName = ""
    , formerLastNames = []
    , age = 29
    , personalId = Passport (1234, 567890) ("XV", "АБ", 123456)
    }

notValid3 :: BetterPerson
notValid3 =
  BetterPerson
    { firstName = "Kate"
    , lastName = "Verbitskaia"
    , formerLastNames = []
    , age = -13
    , personalId = Passport (1234, 567890) ("XV", "АБ", 123456)
    }

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
  assertBool "namesakes" (namesakes validChild1 validChild3)
  assertBool "notNamesakes: same Person" (not $ namesakes validChild3 validChild3WithPassport)
  assertBool "notNamesakes: same Person" (not $ namesakes person1 person1)
  assertBool "notNamesakes" (not $ namesakes person1 validChild1)
  assertBool "notNamesakes: same Person" (not $ namesakes person1 person1Aged)
  assertBool
    "notNamesakes: same Person"
    (not $ namesakes validChild1 validChild2)
  assertBool
    "notNamesakes: different last name"
    (not $ namesakes person1 person3)
  assertBool
    "notNamesakes: different first name"
    (not $ namesakes person1 person4)

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
  assertBool "not valid: no first name" (not $ validatePerson notValid1)
  assertBool "not valid: no last name" (not $ validatePerson notValid2)
  assertBool "not valid: negative age" (not $ validatePerson notValid3)
  -- New tests
  assertBool "valid" (validatePerson validChild1)
  assertBool "not valid: child should not have passport" (not $ validatePerson child1)
  assertBool
    "not valid: incorrect BirthCertificate format (latin)"
    (not $ validatePerson child2)
  assertBool
    "not valid: incorrect BirthCertificate format (cyrillic)"
    (not $ validatePerson child3)
  assertBool
    "not valid: incorrect BirthCertificate format (len)"
    (not $ validatePerson child4)
  assertBool
    "not valid: incorrect BirthCertificate format (numbers)"
    (not $ validatePerson child5)
