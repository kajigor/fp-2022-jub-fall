{-# LANGUAGE InstanceSigs #-}

module Person where

import           MyEq     (MyEq (..))
import           ToString

-- Тип данных для человека
data Person =
  Person
    { firstName       :: String -- Имя, должно быть непустым
    , lastName        :: String -- Фамилия, должна быть непустой
    , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
    , age             :: Int -- Возраст, должен быть неотрицательным
    , idNumber        :: (Int, Int) -- Номер паспорта: состоит из серии и номера.
    } -- -- У детей (людей младше 14 лет) номера паспорта --- (0000, 000000)
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq Person where
  (===) :: Person -> Person -> Bool
  (===) x y = idNumber x === idNumber y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString Person where
  toString :: Person -> String
  toString Person {firstName = _firstName, lastName = _lastName, age = _age} =
    _firstName ++ " " ++ _lastName ++ ", " ++ toString _age

-- Увеличить возраст на 1
ageUp :: Person -> Person
ageUp person = person {age = 1 + age person}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: Person -> String -> Person
updateLastName person newLastName =
  if newLastName == lastName person
    then person
    else person
           { formerLastNames = lastName person : formerLastNames person
           , lastName = newLastName
           }

-- Проверки на корректность (указаны в комментариях к типу данных)
validateIdFormat :: Int -> (Int, Int) -> Bool
validateIdFormat _age (id_series, id_number) =
  if _age < 14
    then id_series == 0 && id_number == 0
    else 0 <= id_series &&
         id_series < 10000 && 0 <= id_number && id_number < 1000000

validatePerson :: Person -> Bool
validatePerson person =
  firstName person /= "" &&
  lastName person /= "" &&
  age person >= 0 && validateIdFormat (age person) (idNumber person)

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: Person -> Person -> Bool
namesakes x y =
  firstName x == firstName y &&
  lastName x == lastName y && idNumber x /= idNumber y
