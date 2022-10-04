{-# LANGUAGE InstanceSigs #-}
module BetterPerson where

import MyEq (MyEq (..))
import ToString

-- Тип данных для человека
data BetterPerson = BetterPerson
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: (Int, Int)      -- Номер паспорта: состоит из серии и номера.
  , birthCerteficateNumber :: (Int, String, Int)
                                -- Номер свидетельства о рождении: сосотит из цифры, 2х букв и 6-значного номера
  }                             -- -- У детей (людей младше 14 лет) номера паспорта --- (0000, 000000)
                                -- -- У взрослых может отсутвовать номер сертификата, тогда он будет --- (0, "", 000000 )
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq BetterPerson where
  (===) :: BetterPerson -> BetterPerson -> Bool
  (===) x y | age x < 14 = birthCerteficateNumber x == birthCerteficateNumber y
            | otherwise = idNumber x === idNumber y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString BetterPerson where
  toString :: BetterPerson -> String
  toString person = firstName person ++ " " ++ lastName person ++ ", " ++ toString (age person)

-- Увеличить возраст на 1
ageUp :: BetterPerson -> BetterPerson
ageUp person = person {age = age person + 1}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: BetterPerson -> String -> BetterPerson
updateLastName person newLastName | lastName person == newLastName = person
                                  | otherwise = person {formerLastNames = lastName person : (formerLastNames person), lastName = newLastName}

-- Проверки на корректность (указаны в комментариях к типу данных)
validateBetterPerson :: BetterPerson -> Bool
validateBetterPerson person = firstName person /= [] && lastName person /= []  && age person >= 0 && (age person >= 14 || (idNumber person == (0, 0) && birthCerteficateNumber person /= (0, "", 0)))

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: BetterPerson -> BetterPerson -> Bool
namesakes x y = firstName x == firstName y && lastName x == lastName y && not (x === y)