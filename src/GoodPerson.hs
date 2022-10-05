{-# LANGUAGE InstanceSigs #-}
module GoodPerson where

import MyEq (MyEq (..))
import ToString

-- Тип данных для человека
data GoodPerson = GoodPerson
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: (Int, Int)      -- Номер паспорта: состоит из серии и номера.
  -- -- У детей (людей младше 14 лет) номера паспорта --- (0000, 000000)
  , birthCertificate :: (String, Int)
  }
  deriving (Show, Eq)


-- У разных людей разные номера паспортов
instance MyEq GoodPerson where
  (===) :: GoodPerson -> GoodPerson -> Bool
  (===) x y = (((age x) >= 14) && (idNumber x === idNumber y) || ((age x) < 14) && (birthCertificate x) == (birthCertificate y))

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString GoodPerson where
  toString :: GoodPerson -> String
  toString person = firstName person ++ " " ++ lastName person ++ ", " ++ toString (age person)

-- Увеличить возраст на 1
ageUp :: GoodPerson -> GoodPerson
ageUp person = person {age = (age person) + 1}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: GoodPerson -> String -> GoodPerson
updateLastName person newLastName
  | newLastName == (lastName person) = person
  | otherwise = person {formerLastNames = lastName person : formerLastNames person, lastName = newLastName}

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: GoodPerson -> Bool
validatePerson person
  | ((firstName person) == "") = False
  | ((lastName person) == "") = False
  | ((age person) < 0) = False
  | ((age person) < 14 && (fst (idNumber person) /= 0 || snd (idNumber person) /= 0)) = False
  | length (fst (birthCertificate person)) /= 2 = False
  | otherwise = True

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: GoodPerson -> GoodPerson -> Bool
namesakes x y
  | (firstName x == firstName y && lastName x == lastName y && not (x === y)) = True
  | otherwise = False