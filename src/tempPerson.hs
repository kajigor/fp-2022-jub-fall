{-# LANGUAGE InstanceSigs #-}
module TempPerson where

import MyEq (MyEq (..))
import ToString

data IdNumber = Passport (Int, Int) | Bc (Int, String)
    deriving (Show, Eq)

instance MyEq IdNumber where
    (===) :: IdNumber -> IdNumber -> Bool
    (===) (Bc _) (Passport _) = False
    (===) (Bc x) (Bc y) = x == y
    (===) (Passport _) (Bc _) = False
    (===) (Passport x) (Passport y) = x === y


-- Тип данных для человека
data TempPerson = TempPerson
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: (Int, Int)      -- Номер паспорта: состоит из серии и номера.
  }                             -- -- У детей (людей младше 14 лет) номера паспорта --- (0000, 000000)
  deriving (Show, Eq)

  
-- У разных людей разные номера паспортов
instance MyEq TempPerson where
  (===) :: TempPerson -> TempPerson -> Bool
  (===) x y = idNumber x === idNumber y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString TempPerson where
  toString :: TempPerson -> String
  toString person =
      firstName person ++ " " ++ lastName person ++ ", " ++ toString (age person)

-- Увеличить возраст на 1
ageUp :: TempPerson -> TempPerson
ageUp person =
  person {age = 1 + age person}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: TempPerson -> String -> TempPerson
updateLastName person newLastName =
  if lastName person == newLastName then person
  else person {formerLastNames = lastName person : formerLastNames person, lastName = newLastName}

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: TempPerson -> Bool
validatePerson person =
  firstName person /= "" && lastName person /= "" && age person >= 0 && (age person >= 14 )

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: TempPerson -> TempPerson -> Bool
namesakes x y =
  (firstName x == firstName y) && (lastName x == lastName y) && not (x === y)