{-# LANGUAGE InstanceSigs #-}
module Human where

import MyEq (MyEq (..))
import ToString
import Data.Maybe
import Data.Char (isDigit, isAlpha)

-- Тип данных для человека
data IDCard = Passport (Int, Int) |
              BirthCertificate (String, Int)  -- Серия: xyy (1 цифра, 2 заглавные латинские буквы)
                                              -- Номер: xxxxxx (цифры)
  deriving (Show, Eq)

data Human = Human
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: IDCard          -- Номер паспорта: состоит из серии и номера.
  }                             -- -- У детей (людей младше 14 лет) --- свидетельство о рождении
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq Human where
  (===) :: Human -> Human -> Bool
  (===) x y = idNumber x == idNumber y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString Human where
  toString :: Human -> String
  toString human = firstName human ++ (' ' : lastName human) ++ (',' : ' ' : toString (age human))

-- Увеличить возраст на 1
ageUp :: Human -> Human
ageUp human = human {age = age human + 1}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: Human -> String -> Human
updateLastName human newLastName
  | newLastName == lastName human = human
  | otherwise = human {formerLastNames = lastName human : formerLastNames human, lastName = newLastName}

-- Проверки на корректность (указаны в комментариях к типу данных)
validateHuman :: Human -> Bool
validateHuman human =
  firstName human /= "" &&
  lastName human /= "" &&
  age human >= 0 &&
  case idNumber human of
    BirthCertificate (digit : letter1 : [letter2], number) ->
        age human < 14 &&
        isDigit digit &&
        'A' <= letter1 && letter1 <= 'Z' &&
        'A' <= letter2 && letter2 <= 'Z' &&
        0 <= number && number <= 999999
    Passport (series, number) ->
        age human >= 14 &&
        0 <= series && series <= 9999 &&
        0 <= number && number <= 999999
    _ -> False



-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: Human -> Human -> Bool
namesakes x y =
  x =/= y &&
  firstName x == firstName y &&
  lastName x == lastName y
