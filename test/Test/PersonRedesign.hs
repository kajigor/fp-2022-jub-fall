module Test.PersonRedesign where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import PersonRedesign
import ToString
import PersonRedesign (Person (formerLastNames), Id (RussianBirthCertificate, RussianPassport), ageOfGettingAPassport)

person1 :: Person
person1 =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , id' = Just $ RussianPassport (1234, 567890) }

person1Aged :: Person
person1Aged =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 30
         , id' = Just $ RussianPassport (1234, 567890) }

person1AgedTwice :: Person
person1AgedTwice =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 31
         , id' = Just $ RussianPassport (1234, 567890) }

person2 :: Person
person2 =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 42
         , id' = Just $ RussianPassport (9876, 543210) }

person3 :: Person
person3 =
  Person { firstName = "Kate"
         , lastName = "Smith"
         , formerLastNames = []
         , age = 21
         , id' = Just $ RussianPassport (2121, 212121) }

person4 :: Person
person4 =
  Person { firstName = "Maria"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 23
         , id' = Just $ RussianPassport (1111, 111111) }

person4NewLastName :: Person
person4NewLastName =
  Person { firstName = "Maria"
         , lastName = "Ivanova"
         , formerLastNames = ["Verbitskaia"]
         , age = 23
         , id' = Just $ RussianPassport (1111, 111111) }

person4NewLastNameNewLastName :: Person
person4NewLastNameNewLastName =
  Person { firstName = "Maria"
         , lastName = "Sidorova"
         , formerLastNames = [ "Ivanova", "Verbitskaia" ]
         , age = 23
         , id' = Just $ RussianPassport (1111, 111111) }

child1 :: Person
child1 =
  Person { firstName = "Ivan"
         , lastName = "Ivanov"
         , formerLastNames = []
         , age = 7
         , id' = Just $ RussianBirthCertificate ("V36", 331112) }

child2 :: Person
child2 =
  Person { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , id' = Just $ RussianBirthCertificate ("III99", 123456) }

child3 :: Person
child3 =
  Person { firstName = "Yagami"
         , lastName = "Light"
         , formerLastNames = []
         , age = ageOfGettingAPassport - 1
         , id' = Just $ RussianBirthCertificate ("L33", 515422) }

child3AgedUp :: Person
child3AgedUp =
  Person { firstName = "Yagami"
         , lastName = "Light"
         , formerLastNames = []
         , age = ageOfGettingAPassport
         , id' = Nothing }

notValid1 :: Person
notValid1 =
  Person { firstName = ""
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = 29
         , id' = Just $ RussianPassport (1234, 567890) }

notValid2 :: Person
notValid2 =
  Person { firstName = "Kate"
         , lastName = ""
         , formerLastNames = []
         , age = 29
         , id' = Just $ RussianPassport (1234, 567890) }

notValid3 :: Person
notValid3 =
  Person { firstName = "Kate"
         , lastName = "Verbitskaia"
         , formerLastNames = []
         , age = -13
         , id' = Just $ RussianPassport (1234, 567890) }

notValid4 :: Person
notValid4 =
  Person { firstName = "Masha"
         , lastName = "Ivanova"
         , formerLastNames = []
         , age = 3
         , id' = Just $ RussianPassport (1234, 567890) }

notValid5 :: Person
notValid5 =
  Person { firstName = "Yagami"
         , lastName = "Light"
         , formerLastNames = []
         , age = ageOfGettingAPassport
         , id' = Just $ RussianBirthCertificate ("L33", 515422) }

notValid6 :: Person
notValid6 =
  Person { firstName = "L"
         , lastName = "Lawliet"
         , formerLastNames = []
         , age = ageOfGettingAPassport - 1
         , id' = Just $ RussianPassport (1985, 311000) }


unit_ageUp = do
  ageUp person1 @?= person1Aged
  ageUp (ageUp person1) @?= person1AgedTwice
  ageUp child3 @?= child3AgedUp

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
  toString child3 @?= "Yagami Light, " ++ show (ageOfGettingAPassport - 1)
  toString child3AgedUp @?= "Yagami Light, " ++ show ageOfGettingAPassport

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
  assertBool "valid" (validatePerson child3AgedUp)

  assertBool "not valid: no first name" (not $ validatePerson notValid1)
  assertBool "not valid: no last name" (not $ validatePerson notValid2)
  assertBool "not valid: negative age" (not $ validatePerson notValid3)
  assertBool "not valid: child with id" (not $ validatePerson notValid4)
  assertBool "not valid: too old to have a birth certificate" (not $ validatePerson notValid5)
  assertBool "not valid: too young to have a passport" (not $ validatePerson notValid6)




















