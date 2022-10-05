{-# LANGUAGE InstanceSigs #-}
module BetterPerson where

import MyEq (MyEq (..))
import ToString

-- Тип данных для личного документа: паспорта или свидетельства о рождении
data IdNumber = PassportNumber Int Int | BirthCertificate String Int

instance MyEq IdNumber where
  (===) :: IdNumber -> IdNumber -> Bool
  (===) (PassportNumber x1 y1) (PassportNumber x2 y2) = x1 == x2 && y1 == y2
  (===) (BirthCertificate x1 y1) (BirthCertificate x2 y2) = x1 == x2 && y1 == y2
  (===) _ _ = False

instance Show IdNumber where
  show :: IdNumber -> String
  show (PassportNumber x y) = show x ++ " " ++ show y
  show (BirthCertificate x y) = x ++ " " ++ show y

instance Eq IdNumber where
  (==) :: IdNumber -> IdNumber -> Bool
  (==) (PassportNumber x1 y1) (PassportNumber x2 y2) = x1 == x2 && y1 == y2
  (==) (BirthCertificate x1 y1) (BirthCertificate x2 y2) = x1 == x2 && y1 == y2
  (==) _ _ = False

-- Тип данных для человека
data BetterPerson = BetterPerson
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: IdNumber      -- Номер паспорта: состоит из серии и номера.
  }                             -- -- У детей (людей младше 14 лет) номера паспорта --- (0000, 000000)
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq BetterPerson where
  (===) :: BetterPerson -> BetterPerson -> Bool
  (===) x y = idNumber x === idNumber y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString BetterPerson where
  toString :: BetterPerson -> String
  toString person =
    firstName person ++ " " ++ lastName person ++ ", " ++ show (age person)

-- Увеличить возраст на 1
ageUp :: BetterPerson -> BetterPerson
ageUp person =
  person { age = (age person) + 1 }

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: BetterPerson -> String -> BetterPerson
updateLastName person newLastName =
  if lastName person == newLastName
    then person
    else person { lastName = newLastName, formerLastNames = lastName person : formerLastNames person }

-- Проверки на корректность (указаны в комментариях к типу данных)
validateBetterPerson :: BetterPerson -> Bool
validateBetterPerson person =
  firstName person /= [] &&
  lastName person /= [] &&
  age person >= 0 &&
  (age person >= 14 && isPassport (idNumber person) || age person < 14 && isBirthCertificate (idNumber person))
  where
    isPassport idNumber = case idNumber of
      (PassportNumber _ _) -> True
      (BirthCertificate _ _) -> False
    isBirthCertificate idNumber = case idNumber of
      (BirthCertificate _ _) -> True
      (PassportNumber _ _) -> False

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: BetterPerson -> BetterPerson -> Bool
namesakes x y
  | x === y = False
  | firstName x /= firstName y = False
  | lastName x /= lastName y = False
  | otherwise = True
