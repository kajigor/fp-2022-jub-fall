{-# LANGUAGE InstanceSigs #-}
module PersonFixed where

import MyEq (MyEq (..))
import ToString

-- id человека - свидетельство о рождении или паспорт
data IdNumber = PassportNo (Int, Int) | BirthCertificateNo (String, Int) deriving (Show, Eq)

instance MyEq IdNumber where
  (===) (BirthCertificateNo x) (BirthCertificateNo y) = x == y
  (===) (PassportNo x) (PassportNo y) = x == y
  (===) _ _ = False

-- Тип данных для человека
data PersonFixed = PersonFixed
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: IdNumber        -- У детей (людей младше 14 лет) - свидетельство о рождении, у взрослых - паспорт, состоят из серии и номера.
  }
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq PersonFixed where
  (===) :: PersonFixed -> PersonFixed -> Bool
  (===) x y = idNumber x === idNumber y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString PersonFixed where
  toString :: PersonFixed -> String
  toString person = firstName person ++ " " ++ lastName person ++ ", " ++ show (age person)

-- Увеличить возраст на 1
ageUp :: PersonFixed -> PersonFixed
ageUp person = PersonFixed (firstName person) (lastName person) (formerLastNames person) (age person + 1) (idNumber person)

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: PersonFixed -> String -> PersonFixed
updateLastName person newLastName =
  if newLastName == lastName person
  then person
  else PersonFixed (firstName person) newLastName (lastName person : formerLastNames person) (age person) (idNumber person)

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: PersonFixed -> Bool
validatePerson person =
  firstName person /= "" &&
  lastName person /= "" &&
  age person >= 0 &&
  validateIdNumber person

-- Если человек младше 14 лет
validateIdNumber :: PersonFixed -> Bool
validateIdNumber person =
  if age person < 14 then
    case idNumber person of
    (BirthCertificateNo _) -> True
    _                                              -> False
  else
    case idNumber person of
    (PassportNo _) -> True
    _                                      -> False

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: PersonFixed -> PersonFixed -> Bool
namesakes x y =
  firstName x == firstName y &&
  lastName x == lastName y &&
  idNumber x /= idNumber y