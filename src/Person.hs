{-# LANGUAGE InstanceSigs #-}
module Person where

import MyEq (MyEq (..))
import ToString
import Data.Maybe

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , passport :: Maybe (Int, Int)       -- Номер паспорта: состоит из серии (4 цифры) и номера (6 цифр)
  , birthCertificate :: (String, Int) -- Свидетельство о рождении: серия из римской цифры и 2 букв, номер из 6 цифр
  }                             
  deriving (Show, Eq)

-- У разных людей разные номера 
instance MyEq Person where
  (===) :: Person -> Person -> Bool
  (===) x y = passport x == passport y && birthCertificate x == birthCertificate y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString Person where
  toString :: Person -> String
  toString person = firstName person ++ " " ++ lastName person ++ ", " ++ show (age person)

-- Увеличить возраст на 1
ageUp :: Person -> Person
ageUp person = person { age = (age person + 1) }

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: Person -> String -> Person
updateLastName person newLastName =
  if (lastName person /= newLastName)
    then person { lastName = newLastName, formerLastNames = lastName person : formerLastNames person }
    else person

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: Person -> Bool
validatePerson person = 
  firstName person /= "" && lastName person /= "" && age person >= 0 && 
  ((age person < 14 && isNothing (passport person)) || (age person >= 14 && isJust (passport person)))
  

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: Person -> Person -> Bool
namesakes x y = (firstName x == firstName y && lastName x == lastName y && birthCertificate x /= birthCertificate y)
