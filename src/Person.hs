{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NumericUnderscores #-}
module Person where

import MyEq (MyEq (..))
import ToString

data Identificator = PassportId (Int,Int) | BirthCertificateId Int deriving (Show, Eq)
-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: Identificator      -- Номер паспорта: состоит из серии и номера.
  }                             -- У детей (людей младше 14 лет) BirthCertificateId - шестизначное число
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq Person where
  (===) :: Person -> Person -> Bool
  (===) x y = idNumber x == idNumber y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString Person where
  toString :: Person -> String
  toString person =
    firstName person ++ " " ++ lastName person ++ ", " ++ toString (age person)

-- Увеличить возраст на 1
ageUp :: Person -> Person
ageUp person =
  person {age = age person + 1}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: Person -> String -> Person
updateLastName person newLastName | lastName person == newLastName = person
                                  | otherwise =
                                    person {formerLastNames = lastName person : formerLastNames person, lastName = newLastName}


isPassportId :: Identificator -> Bool
isPassportId (PassportId (_,_)) = True
isPassportId _ = False

hasPassport :: Person -> Bool
hasPassport = isPassportId . idNumber

isValidIdentificator :: Identificator -> Bool
isValidIdentificator (PassportId (ser,num)) = (0 <= ser && ser <= 9999) && (0 <= num && num <= 99_999_999)
isValidIdentificator (BirthCertificateId id) = 0 <= id && id <= 999_999

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: Person -> Bool
validatePerson person =
  not (null $ firstName person) &&
  not (null $ lastName person) &&
  age person >= 0 &&
  hasPassport person == (age person > 14) 
  && isValidIdentificator (idNumber person) 
  && notElem (lastName person) (formerLastNames person) -- formerLastNames can't contain current lastName




-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: Person -> Person -> Bool
namesakes x y =
  firstName x == firstName y &&
  lastName x == lastName y &&
  x =/= y
