module Test.NewPerson where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import NewPerson
    (NewPerson (AdultPerson, ChildPerson), Adult(..), Child(..), ageUp, namesakes, updateLastName, validatePerson )
import ToString

person1 :: NewPerson
person1 =
  AdultPerson Adult { aFirstName = "Kate"
         , aLastName = "Verbitskaia"
         , aFormerLastNames = []
         , aAge = 29
         , aIdNumber = (1234, 567890) }

person1Aged :: NewPerson
person1Aged =
  AdultPerson Adult { aFirstName = "Kate"
         , aLastName = "Verbitskaia"
         , aFormerLastNames = []
         , aAge = 30
         , aIdNumber = (1234, 567890) }

person1AgedTwice :: NewPerson
person1AgedTwice =
  AdultPerson Adult { aFirstName = "Kate"
         , aLastName = "Verbitskaia"
         , aFormerLastNames = []
         , aAge = 31
         , aIdNumber = (1234, 567890) }

person2 :: NewPerson
person2 =
  AdultPerson Adult { aFirstName = "Kate"
         , aLastName = "Verbitskaia"
         , aFormerLastNames = []
         , aAge = 42
         , aIdNumber = (9876, 543210) }

person3 :: NewPerson
person3 =
  AdultPerson Adult { aFirstName = "Kate"
         , aLastName = "Smith"
         , aFormerLastNames = []
         , aAge = 21
         , aIdNumber = (2121, 212121) }

person4 :: NewPerson
person4 =
  AdultPerson Adult { aFirstName = "Maria"
         , aLastName = "Verbitskaia"
         , aFormerLastNames = []
         , aAge = 23
         , aIdNumber = (1111, 111111) }

person4NewLastName :: NewPerson
person4NewLastName =
  AdultPerson Adult { aFirstName = "Maria"
         , aLastName = "Ivanova"
         , aFormerLastNames = ["Verbitskaia"]
         , aAge = 23
         , aIdNumber = (1111, 111111) }

person4NewLastNameNewLastName :: NewPerson
person4NewLastNameNewLastName =
  AdultPerson Adult { aFirstName = "Maria"
         , aLastName = "Sidorova"
         , aFormerLastNames = [ "Ivanova", "Verbitskaia" ]
         , aAge = 23
         , aIdNumber = (1111, 111111) }

child1 :: NewPerson
child1 =
  ChildPerson Child { cFirstName = "Ivan"
         , cLastName = "Ivanov"
         , cFormerLastNames = []
         , cAge = 7 }

child2 :: NewPerson
child2 =
  ChildPerson Child { cFirstName = "Masha"
         , cLastName = "Ivanova"
         , cFormerLastNames = []
         , cAge = 3 }

notValid1 :: NewPerson
notValid1 =
  AdultPerson Adult { aFirstName = ""
         , aLastName = "Verbitskaia"
         , aFormerLastNames = []
         , aAge = 29
         , aIdNumber = (1234, 567890) }

notValid2 :: NewPerson
notValid2 =
  AdultPerson Adult { aFirstName = "Kate"
         , aLastName = ""
         , aFormerLastNames = []
         , aAge = 29
         , aIdNumber = (1234, 567890) }

notValid3 :: NewPerson
notValid3 =
  AdultPerson Adult { aFirstName = "Kate"
         , aLastName = "Verbitskaia"
         , aFormerLastNames = []
         , aAge = -13
         , aIdNumber = (1234, 567890) }

notValid4 :: NewPerson
notValid4 =
  AdultPerson Adult { aFirstName = "Masha"
         , aLastName = "Ivanova"
         , aFormerLastNames = []
         , aAge = 3
         , aIdNumber = (1234, 567890) }


unit_ageUp = do
  ageUp person1 @?= person1Aged
  ageUp (ageUp person1) @?= person1AgedTwice

unit_updateLastName = do
  let person4' = updateLastName person4 "Ivanova"
  let person4'' = updateLastName person4NewLastName "Sidorova"
  person4' @?= person4NewLastName
  person4'' @?= person4NewLastNameNewLastName
  let AdultPerson adultPerson4 = person4
  person4 @?= updateLastName person4 (aLastName adultPerson4)


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


















