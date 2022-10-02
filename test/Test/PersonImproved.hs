module Test.PersonImproved where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import PersonImproved
import MyEq

person1 :: PersonImproved
person1 =
  PersonImproved { firstName = "Kate"
                 , lastName = "Verbitskaia"
                 , formerLastNames = []
                 , age = 29
                 , idNumber = (1234, 567890)
                 , birthCertificateNumber = (0, "", 0)}

person2 :: PersonImproved
person2 =
  PersonImproved { firstName = "Kate"
                 , lastName = "Verbitskaia"
                 , formerLastNames = []
                 , age = 29
                 , idNumber = (1234, 567890)
                 , birthCertificateNumber = (12, "AK", 345678)}

child1 :: PersonImproved
child1 =
  PersonImproved { firstName = "Kate"
                 , lastName = "Verbitskaia"
                 , formerLastNames = []
                 , age = 4
                 , idNumber = (0, 0)
                 , birthCertificateNumber = (12, "AK", 345678)}

child1Aged :: PersonImproved
child1Aged =
  PersonImproved { firstName = "Kate"
                 , lastName = "Verbitskaia"
                 , formerLastNames = []
                 , age = 5
                 , idNumber = (0, 0)
                 , birthCertificateNumber = (12, "AK", 345678)}

child1Adult :: PersonImproved
child1Adult =
  PersonImproved { firstName = "Kate"
                 , lastName = "Verbitskaia"
                 , formerLastNames = []
                 , age = 15
                 , idNumber = (1234, 567890)
                 , birthCertificateNumber = (12, "AK", 345678)}

notValid1 :: PersonImproved
notValid1 =
  PersonImproved { firstName = "Kate"
                 , lastName = "Verbitskaia"
                 , formerLastNames = []
                 , age = 4
                 , idNumber = (1234, 567890)
                 , birthCertificateNumber = (12, "AK", 345678)}

notValid2 :: PersonImproved
notValid2 =
  PersonImproved { firstName = "Kate"
                 , lastName = "Verbitskaia"
                 , formerLastNames = []
                 , age = 4
                 , idNumber = (0, 0)
                 , birthCertificateNumber = (0, "", 0)}

unit_eq = do
  assertBool "equals: child aged" (child1 === child1Aged)
  assertBool "equals: child became adult" (child1 === child1Aged)

unit_valid = do
  assertBool "valid" (validatePerson person1)
  assertBool "valid" (validatePerson person2)
  assertBool "valid" (validatePerson person2)
  assertBool "valid" (validatePerson child1)
  assertBool "valid" (validatePerson child1Aged)
  assertBool "valid" (validatePerson child1Adult)

  assertBool "not valid: child with id" (not $ validatePerson notValid1)
  assertBool "not valid: child without birth certificate" (not $ validatePerson notValid2)
