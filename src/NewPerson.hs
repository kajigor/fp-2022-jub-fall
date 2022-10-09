{-# LANGUAGE InstanceSigs #-}
module NewPerson where

import MyEq (MyEq (..))
import ToString
import Data.Bool (Bool(False))

data ID = BirthCertificate String Int | Passport Int Int deriving (Show, Eq)

isBirthCertificate :: ID -> Bool
isBirthCertificate doc = case doc of
  BirthCertificate _ _ -> True
  Passport _ _ -> False


-- Тип данных для человека
data NewPerson = NewPerson
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idDocument :: ID            -- Документ, удостоверяющий личность
  }  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq NewPerson where
  (===) :: NewPerson -> NewPerson -> Bool
  (===) x y = idDocument x == idDocument y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString NewPerson where
  toString :: NewPerson -> String
  toString x = firstName x ++ " " ++ lastName x ++ ", " ++ show (age x)

-- Увеличить возраст на 1
ageUp :: NewPerson -> NewPerson
ageUp person = person{ age = age person + 1}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: NewPerson -> String -> NewPerson
updateLastName person newLastName
  | lastName person == newLastName = person
  | otherwise = person{ lastName = newLastName, formerLastNames = lastName person : formerLastNames person}

-- Проверки на корректность (указаны в комментариях к типу данных)
validateNewPerson :: NewPerson -> Bool
validateNewPerson person = firstName person /= "" && lastName person /= "" && age person >= 0 && (age person <= 14) == (isBirthCertificate $ idDocument person)

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: NewPerson -> NewPerson -> Bool
namesakes x y = x =/= y && (firstName x, lastName x) == (firstName y, lastName y)