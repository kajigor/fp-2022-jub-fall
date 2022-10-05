module Test.NewPerson where

import NewPerson
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import ToString

person1 :: NewPerson
person1 =
  NewPerson
    { firstName = "Kate",
      lastName = "Verbitskaia",
      formerLastNames = [],
      age = 29,
      -- this is ugly, how can I change it?
      idDoc = Passport_ (Passport (1234, 567890))
    }

person1Aged :: NewPerson
person1Aged =
  NewPerson
    { firstName = "Kate",
      lastName = "Verbitskaia",
      formerLastNames = [],
      age = 30,
      idDoc = Passport_ (Passport (1234, 567890))
    }

person1AgedTwice :: NewPerson
person1AgedTwice =
  NewPerson
    { firstName = "Kate",
      lastName = "Verbitskaia",
      formerLastNames = [],
      age = 31,
      idDoc = Passport_ (Passport (1234, 567890))
    }

person2 :: NewPerson
person2 =
  NewPerson
    { firstName = "Kate",
      lastName = "Verbitskaia",
      formerLastNames = [],
      age = 42,
      idDoc = Passport_ (Passport (9876, 543210))
    }

person3 :: NewPerson
person3 =
  NewPerson
    { firstName = "Kate",
      lastName = "Smith",
      formerLastNames = [],
      age = 21,
      idDoc = Passport_ (Passport (2121, 212121))
    }

person4 :: NewPerson
person4 =
  NewPerson
    { firstName = "Maria",
      lastName = "Verbitskaia",
      formerLastNames = [],
      age = 23,
      idDoc = Passport_ (Passport (1111, 111111))
    }

person4NewLastName :: NewPerson
person4NewLastName =
  NewPerson
    { firstName = "Maria",
      lastName = "Ivanova",
      formerLastNames = ["Verbitskaia"],
      age = 23,
      idDoc = Passport_ (Passport (1111, 111111))
    }

person4NewLastNameNewLastName :: NewPerson
person4NewLastNameNewLastName =
  NewPerson
    { firstName = "Maria",
      lastName = "Sidorova",
      formerLastNames = ["Ivanova", "Verbitskaia"],
      age = 23,
      idDoc = Passport_ (Passport (1111, 111111))
    }

child1 :: NewPerson
child1 =
  NewPerson
    { firstName = "Ivan",
      lastName = "Ivanov",
      formerLastNames = [],
      age = 7,
      idDoc = BirthCert_ (BirthCert ("1CP", 123456))
    }

child2 :: NewPerson
child2 =
  NewPerson
    { firstName = "Masha",
      lastName = "Ivanova",
      formerLastNames = [],
      age = 3,
      idDoc = BirthCert_ (BirthCert ("3RI", 654321))
    }

notValid1 :: NewPerson
notValid1 =
  NewPerson
    { firstName = "",
      lastName = "Verbitskaia",
      formerLastNames = [],
      age = 29,
      idDoc = BirthCert_ (BirthCert ("OPS", 123456))
    }

notValid2 :: NewPerson
notValid2 =
  NewPerson
    { firstName = "Kate",
      lastName = "",
      formerLastNames = [],
      age = 29,
      idDoc = Passport_ (Passport (1111, 222222))
    }

notValid3 :: NewPerson
notValid3 =
  NewPerson
    { firstName = "Kate",
      lastName = "Verbitskaia",
      formerLastNames = [],
      age = -13,
      idDoc = Passport_ (Passport (1111, 222222))
    }

notValid4 :: NewPerson
notValid4 =
  NewPerson
    { firstName = "Masha",
      lastName = "Ivanova",
      formerLastNames = [],
      age = 3,
      idDoc = Passport_ (Passport (123, 456789))
    }

strangePerson :: NewPerson
strangePerson =
  NewPerson
    { firstName = "Kotik",
      lastName = "Vasin",
      formerLastNames = ["Petin", "Mashin", "Dashin", "Kolin"],
      age = 103,
      idDoc = OtherDoc_ (OtherDoc "tail" "stripped, long, grey and white")
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
  toString strangePerson @?= "Kotik Vasin, 103"

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
  assertBool "valid" (validatePerson strangePerson)

  assertBool "not valid: no first name" (not $ validatePerson notValid1)
  assertBool "not valid: no last name" (not $ validatePerson notValid2)
  assertBool "not valid: negative age" (not $ validatePerson notValid3)
  assertBool "not valid: child with id" (not $ validatePerson notValid4)
