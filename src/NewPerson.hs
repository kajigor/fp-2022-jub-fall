{-# LANGUAGE InstanceSigs #-}
module NewPerson where

import MyEq (MyEq (..))
import ToString

-- Тип данных для человека

data DocumentType = Adult { idNumber :: (Int, Int) } | Child { certOfBirth :: (Char, Char, Int) }
    deriving (Show, Eq)

data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , document :: DocumentType
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  }                             -- -- У детей (людей младше 14 лет) номера паспорта --- (0000, 000000)
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq DocumentType where
  (===) :: DocumentType -> DocumentType -> Bool
  (===) (Adult x) (Adult y) = x == y
  (===) (Child x) (Child y) = x == y
  (===) _ _ = False

instance MyEq Person where
  (===) :: Person -> Person -> Bool
  (===) x y = document x == document y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString Person where
  toString :: Person -> String
  toString person =
    firstName person ++ " " ++ lastName person ++ ", " ++ show (age person)

-- Увеличить возраст на 1
ageUp :: Person -> Person
ageUp person =
  if age person == 13
    then error "For 13 y.o. use ageUpFrom13To14 and provide new passport"
    else person { age = age person + 1 }

ageUpFrom13To14 :: Person -> (Int, Int) -> Person
ageUpFrom13To14 person id =
  if age person /= 13
    then error "Use this function only for 13 y.o."
    else person { age = 14, document = Adult id }

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: Person -> String -> Person
updateLastName person newLastName =
  if newLastName == lastName person
    then person
    else person { formerLastNames = lastName person : formerLastNames person, lastName = newLastName }

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: Person -> Bool
validatePerson person =
  firstName person /= "" &&
  lastName person /= "" &&
  age person >= 0 &&
  case document person of
    Adult x -> age person >= 14
    Child x -> age person < 14

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: Person -> Person -> Bool
namesakes x y =
  firstName x == firstName y &&
  lastName x == lastName y &&
  document x /= document y
