{-# LANGUAGE InstanceSigs #-}
module NewPerson where

import MyEq (MyEq (..))
import ToString
import Control.Arrow (Arrow(first))
import Control.Concurrent (yield)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , birthCertificateNumber :: (String, Int) -- Номер свидетельства о рождении: состоит из серии и номера, не должен быть пустым
  , idNumber :: (Int, Int)      -- Номер паспорта: состоит из серии и номера.
  }                             -- -- У детей (людей младше 14 лет) номера паспорта --- (0000, 000000)
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq Person where
  (===) :: Person -> Person -> Bool
  (===) x y | age x >= 14 && age y >= 14 = idNumber x === idNumber y
            | age x < 14 && age y < 14 = birthCertificateNumber x == birthCertificateNumber y 
            | otherwise = False

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString Person where
  toString :: Person -> String
  toString person =
    firstName person ++ " " ++ lastName person ++ ", " ++ show (age person)

ageUp :: Person -> (Int, Int) -> Person
ageUp person id
    | age person == 13 = person { age = age person + 1, idNumber = id }
    | otherwise = person { age = age person + 1 }

validatePerson :: Person -> Bool
validatePerson person =
  firstName person /= "" &&
  lastName person /= "" &&
  age person >= 0 &&
  ((age person < 14 && idNumber person == (0000, 000000)) ||
  (age person >= 14 && idNumber person /= (0000, 000000))) &&
  birthCertificateNumber person /= ("", 000000)
  
-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: Person -> String -> Person
updateLastName person newLastName
  | newLastName == lastName person = person 
  | otherwise = person { formerLastNames = lastName person : formerLastNames person, lastName = newLastName }

namesakes :: Person -> Person -> Bool
namesakes x y =
  (firstName x == firstName y) && (lastName x == lastName y) && (birthCertificateNumber x /= birthCertificateNumber y)