module Test.NewPerson where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import NewPerson
    (NewPerson (AdultPerson, ChildPerson), Adult(..), Child(..), ageUp, namesakes, updateLastName, validatePerson )
import ToString

person1 :: NewPerson
person1 =
  AdultPerson Adult { personData = Child { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29 }
         , idNumber = (1234, 567890) }

person1Aged :: NewPerson
person1Aged =
  AdultPerson Adult { personData = Child {firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 30 }
         , idNumber = (1234, 567890) }

person1AgedTwice :: NewPerson
person1AgedTwice =
  AdultPerson Adult { personData = Child { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 31 }
         , idNumber = (1234, 567890) }

person2 :: NewPerson
person2 =
  AdultPerson Adult { personData = Child { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42 }
         , idNumber = (9876, 543210) }

person3 :: NewPerson
person3 =
  AdultPerson Adult { personData = Child {firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21 }
         , idNumber = (2121, 212121) }

person4 :: NewPerson
person4 =
  AdultPerson Adult { personData = Child { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 23 }
         , idNumber = (1111, 111111) }

person4NewLastName :: NewPerson
person4NewLastName =
  AdultPerson Adult { personData = Child {firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = ["Verbitskaia"]
         , age = 23 }
         , idNumber = (1111, 111111) }

person4NewLastNameNewLastName :: NewPerson
person4NewLastNameNewLastName =
  AdultPerson Adult { personData = Child { firstName = "Maria"
         , lastName = "Sidorova"
         , formerLastNames = [ "Ivanova", "Verbitskaia" ]
         , age = 23 }
         , idNumber = (1111, 111111) }

child1 :: NewPerson
child1 =
  ChildPerson Child { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7 }

child2 :: NewPerson
child2 =
  ChildPerson Child { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3 }

notValid1 :: NewPerson
notValid1 =
  AdultPerson Adult { personData = Child {firstName = ""
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29 }
         , idNumber = (1234, 567890) }

notValid2 :: NewPerson
notValid2 =
  AdultPerson Adult { personData = Child { firstName = "Kate"
         , lastName = ""
         , formerLastNames = []
         , age = 29 }
         , idNumber = (1234, 567890) }

notValid3 :: NewPerson
notValid3 =
  AdultPerson Adult { personData = Child {firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = -13 }
         , idNumber = (1234, 567890) }

notValid4 :: NewPerson
notValid4 =
  AdultPerson Adult { personData = Child {firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3 }
         , idNumber = (1234, 567890) }

notValidChild1 :: NewPerson
notValidChild1 =
  ChildPerson Child { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 14 }
child3 :: NewPerson
child3 = ChildPerson Child { firstName = "Sergey"
         , lastName = "KhB"
         , formerLastNames = []
         , age = 13}
person5 :: NewPerson
person5 = AdultPerson Adult { personData = Child{
  firstName = "Sergey",
  lastName = "KhB",
  formerLastNames = ["KekLol"],
  age = 1337
}, idNumber = (1234, 567890)}


unit_ageUp = do
  ageUp person1 @?= person1Aged
  ageUp (ageUp person1) @?= person1AgedTwice

unit_updateLastName = do
  let person4' = updateLastName person4 "Ivanova"
  let person4'' = updateLastName person4NewLastName "Sidorova"
  person4' @?= person4NewLastName
  person4'' @?= person4NewLastNameNewLastName
  let AdultPerson adultPerson4 = person4
  person4 @?= updateLastName person4 ((lastName . personData) adultPerson4)


unit_namesakes = do
  assertBool "namesakes" (namesakes person1 person2)
  assertBool "notNamesakes: same person" (not $ namesakes person1 person1)
  assertBool "notNamesakes: same person" (not $ namesakes person1 person1Aged)
  assertBool "notNamesakes: different last name" (not $ namesakes person1 person3)
  assertBool "notNamesakes: different first name" (not $ namesakes person1 person4)
  assertBool "namesakes Adult Child" (namesakes person5 child3)
  assertBool "namesakes Child Adult" (namesakes child3 person5)
  assertBool "not namesakes Adult Child" (not $ namesakes person5 child2)
  assertBool "not namesakes Child Adult" (not $ namesakes child2 person5)

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
  assertBool "valid" (validatePerson child3)
  assertBool "valid" (validatePerson (ageUp child3))

  assertBool "not valid: no first name" (not $ validatePerson notValid1)
  assertBool "not valid: no last name" (not $ validatePerson notValid2)
  assertBool "not valid: negative age" (not $ validatePerson notValid3)
  assertBool "not valid: child with id" (not $ validatePerson notValid4)
  assertBool "not valid: too old child" (not $ validatePerson notValidChild1)

















