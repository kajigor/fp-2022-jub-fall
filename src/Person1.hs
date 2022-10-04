{-# LANGUAGE InstanceSigs #-}
module Person1 where

import MyEq (MyEq (..))
import ToString

-- Тип данных для удостоверения личности
data IdNumber = Passport (Int, Int)   -- Номер паспорта: состоит из серии и номера.
    | BirthSertificate ([Char], Int)  -- Номер свидетельсята о рождении: состоит из серии и номера.
    deriving (Show, Eq)

instance MyEq IdNumber where
    (===) :: IdNumber -> IdNumber -> Bool
    (===) (Passport _) (BirthSertificate _) = False
    (===) (BirthSertificate _) (Passport _) = False
    (===) (Passport x) (Passport y) = x === y
    (===) (BirthSertificate x) (BirthSertificate y) = x == y

isPassport :: IdNumber -> Bool
isPassport (Passport _) = True
isPassport (BirthSertificate _) = False

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: IdNumber        -- Номер документа, удостоверяющего личность,
  }                             -- -- У детей (людей младше 14 лет) это свидетельство о рождении,
  deriving (Show, Eq)           -- -- У взрослых - паспорт.

-- У разных людей разные номера паспортов
instance MyEq Person where
  (===) :: Person -> Person -> Bool
  (===) x y = idNumber x === idNumber y

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
updateLastName person newLastName =
  if newLastName == lastName person then person
  else person {formerLastNames =lastName person : formerLastNames person, lastName = newLastName}

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: Person -> Bool
validatePerson person =
  and [firstName person /= "", lastName person /= "", age person >= 0, (age person >= 14) == isPassport (idNumber person)]

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: Person -> Person -> Bool
namesakes x y =
  not (x === y) && (firstName x == firstName y) && (lastName x == lastName y)