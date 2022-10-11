module Test.NextLevelPerson where

import Data.Maybe (fromJust)
import NextLevelPerson
  ( Document (..),
    DocumentHolder (..),
    NextLevelPerson (..),
    addDocument,
    addPassport,
    ageUp,
    getDocument,
    namesakes,
    updateLastName,
    validatePerson,
  )
import Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import ToString

age1 = 29

documents1 = addDocument age1 DocumentHolder {passports = [], birthCertificates = []}

document1 = Just (getDocument age1 documents1)

person1 :: NextLevelPerson
person1 =
  NextLevelPerson
    { firstName = "Kate",
      lastName = "Verbitskaia",
      formerLastNames = [],
      age = age1,
      document = document1,
      formerDocuments = []
    }

person1Aged :: NextLevelPerson
person1Aged =
  NextLevelPerson
    { firstName = "Kate",
      lastName = "Verbitskaia",
      formerLastNames = [],
      age = 30,
      document = document1,
      formerDocuments = []
    }

person1AgedTwice :: NextLevelPerson
person1AgedTwice =
  NextLevelPerson
    { firstName = "Kate",
      lastName = "Verbitskaia",
      formerLastNames = [],
      age = 31,
      document = document1,
      formerDocuments = []
    }

age2 = 42

documents2 = addDocument age2 documents1

document2 = Just (getDocument age2 documents2)

person2 :: NextLevelPerson
person2 =
  NextLevelPerson
    { firstName = "Kate",
      lastName = "Verbitskaia",
      formerLastNames = [],
      age = age2,
      document = document2,
      formerDocuments = []
    }

age3 = 21

documents3 = addDocument age3 documents2

document3 = Just (getDocument age3 documents3)

person3 :: NextLevelPerson
person3 =
  NextLevelPerson
    { firstName = "Kate",
      lastName = "Smith",
      formerLastNames = [],
      age = age3,
      document = document3,
      formerDocuments = []
    }

age4 = 23

documents4 = addDocument age4 documents3

document4 = Just (getDocument age4 documents4)

person4 :: NextLevelPerson
person4 =
  NextLevelPerson
    { firstName = "Maria",
      lastName = "Verbitskaia",
      formerLastNames = [],
      age = age4,
      document = document4,
      formerDocuments = []
    }

documents4NewLastName = addDocument age4 documents4

document4NewLastName = Just (getDocument age4 documents4NewLastName)

person4NewLastName :: NextLevelPerson
person4NewLastName =
  NextLevelPerson
    { firstName = "Maria",
      lastName = "Ivanova",
      formerLastNames = ["Verbitskaia"],
      age = age4,
      document = document4NewLastName,
      formerDocuments = [fromJust document4]
    }

documents4NewLastNameNewLastName = addDocument age4 documents4NewLastName

document4NewLastNameNewLastName = Just (getDocument age4 documents4NewLastNameNewLastName)

person4NewLastNameNewLastName :: NextLevelPerson
person4NewLastNameNewLastName =
  NextLevelPerson
    { firstName = "Maria",
      lastName = "Sidorova",
      formerLastNames = ["Ivanova", "Verbitskaia"],
      age = age4,
      document = document4NewLastNameNewLastName,
      formerDocuments = []
    }

child1Age = 7

documentsChild1 = addDocument child1Age documents4NewLastNameNewLastName

documentChild1 = Just (getDocument child1Age documentsChild1)

child1 :: NextLevelPerson
child1 =
  NextLevelPerson
    { firstName = "Ivan",
      lastName = "Ivanov",
      formerLastNames = [],
      age = child1Age,
      document = documentChild1,
      formerDocuments = []
    }

child2Age = 3

documentsChild2 = addDocument child2Age documentsChild1

documentChild2 = Just (getDocument child2Age documentsChild2)

child2 :: NextLevelPerson
child2 =
  NextLevelPerson
    { firstName = "Masha",
      lastName = "Ivanova",
      formerLastNames = [],
      age = child2Age,
      document = documentChild2,
      formerDocuments = []
    }

notValid1Age = 29

documentsnotValid1 = addDocument notValid1Age documentsChild2

documentnotValid1 = Just (getDocument notValid1Age documentsnotValid1)

notValid1 :: NextLevelPerson
notValid1 =
  NextLevelPerson
    { firstName = "",
      lastName = "Verbitskaia",
      formerLastNames = [],
      age = notValid1Age,
      document = documentnotValid1,
      formerDocuments = []
    }

notValid2Age = 29

documentsnotValid2 = addDocument notValid2Age documentsnotValid1

documentnotValid2 = Just (getDocument notValid2Age documentsnotValid2)

notValid2 :: NextLevelPerson
notValid2 =
  NextLevelPerson
    { firstName = "Kate",
      lastName = "",
      formerLastNames = [],
      age = notValid2Age,
      document = documentnotValid2,
      formerDocuments = []
    }

notValid3Age = -13

documentsnotValid3 = addDocument notValid3Age documentsnotValid2

documentnotValid3 = Just (getDocument notValid3Age documentsnotValid3)

notValid3 :: NextLevelPerson
notValid3 =
  NextLevelPerson
    { firstName = "Kate",
      lastName = "Verbitskaia",
      formerLastNames = [],
      age = -13,
      document = documentnotValid3,
      formerDocuments = []
    }

notValid4Age = 3

documentsnotValid4 = addDocument 14 documentsnotValid2 -- требуется указать возраст от 14 лет чтобы добавить и получить паспорт

documentnotValid4 = Just (getDocument 14 documentsnotValid4)

notValid4 :: NextLevelPerson
notValid4 =
  NextLevelPerson
    { firstName = "Masha",
      lastName = "Ivanova",
      formerLastNames = [],
      age = notValid4Age,
      document = documentnotValid4,
      formerDocuments = []
    }

documentsAll1 = addDocument (age person1) documentsnotValid4

documentsAll2 = addDocument (age person1) documentsAll1

unit_ageUp = do
  ageUp person1 documentsAll1 @?= person1Aged
  ageUp (ageUp person1 documentsAll1) documentsAll2 @?= person1AgedTwice

documentsAll3 = addDocument (age person4) documentsAll2

documentsAll4 = addDocument (age person4) documentsAll3

documentsAll5 = addDocument (age person4) documentsAll4

unit_updateLastName = do
  let person4' = updateLastName person4 "Ivanova" documentsAll3
  let person4'' = updateLastName person4' "Sidorova" documentsAll4
  person4'
    @?= NextLevelPerson
      { firstName = "Maria",
        lastName = "Ivanova",
        formerLastNames = ["Verbitskaia"],
        age = age person4,
        document = Just (getDocument (age person4) documentsAll3),
        formerDocuments = [fromJust (document person4)]
      }
  person4''
    @?= NextLevelPerson
      { firstName = "Maria",
        lastName = "Sidorova",
        formerLastNames = ["Ivanova","Verbitskaia"],
        age = age person4,
        document = Just (getDocument (age person4) documentsAll4),
        formerDocuments = [fromJust (document person4'), fromJust (document person4)]
      }
  person4 @?= updateLastName person4 (lastName person4) documentsAll5

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
  assertBool "valid" (validatePerson person1 documentsAll5)
  assertBool "valid" (validatePerson person1Aged documentsAll5)
  assertBool "valid" (validatePerson person1AgedTwice documentsAll5)
  assertBool "valid" (validatePerson person2 documentsAll5)
  assertBool "valid" (validatePerson person3 documentsAll5)
  assertBool "valid" (validatePerson person4 documentsAll5)
  assertBool "valid" (validatePerson person4NewLastName documentsAll5)
  assertBool "valid" (validatePerson person4NewLastNameNewLastName documentsAll5)
  assertBool "valid" (validatePerson child1 documentsAll5)
  assertBool "valid" (validatePerson child2 documentsAll5)

  assertBool "not valid: no first name" (not $ validatePerson notValid1 documentsAll5)
  assertBool "not valid: no last name" (not $ validatePerson notValid2 documentsAll5)
  assertBool "not valid: negative age" (not $ validatePerson notValid3 documentsAll5)
  assertBool "not valid: child with id" (not $ validatePerson notValid4 documentsAll5)
