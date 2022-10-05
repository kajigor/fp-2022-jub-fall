{-# LANGUAGE InstanceSigs #-}
module BetterPerson where

import ToString

-- Тип данных для человека
data BetterPerson = BetterPerson
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: (Int, Int)      -- Номер паспорта для лиц старше 14 лет: состоит из серии и номера.
  , birthCert :: (String, Int)  -- Номер свид-ва о рождении для лиц младше 14 лет: две буквы + номер.
  }
  deriving (Show, Eq)

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString BetterPerson where
  toString :: BetterPerson -> String
  toString person = firstName person ++ " " ++ lastName person ++ ", " ++ show (age person)

-- Увеличить возраст на 1
ageUp :: BetterPerson -> BetterPerson
ageUp person =
  person { age = age person + 1 }

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: BetterPerson -> String -> BetterPerson
updateLastName person newLastName | lastName person == newLastName = person
                                  | otherwise = person { formerLastNames = (lastName person) : (formerLastNames person)
                                                       , lastName = newLastName }

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: BetterPerson -> Bool
validatePerson person =
       firstName person /= ""
    && lastName person /= ""
    && age person >= 0
    &&    (age person < 14 && birthCert person /= ("", 0) && idNumber person == (0, 0)
        || age person >= 14 && birthCert person /= ("", 0) && idNumber person /= (0, 0))


-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: BetterPerson -> BetterPerson -> Bool
namesakes x y =
       firstName x == firstName y
    && lastName x == lastName y
    && (age x <= 14 && idNumber x /= idNumber y || age x > 14 && birthCert x /= birthCert y)
