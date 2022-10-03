{-# LANGUAGE InstanceSigs #-}
module ImprovedPerson where

import MyEq (MyEq (..))
import ToString

-- Тип данных для человека
data ImprovedPerson = ImprovedPerson
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , certificateNumber :: (String, Int)  -- Номер свидетельства о рождении: состоит из серии (непустая строка) и номера
  }                             -- Верим, что хотя бы свидетельство о рождении у всех есть
  deriving (Show, Eq)

-- У разных людей разные номера свидетельств
instance MyEq ImprovedPerson where
  (===) :: ImprovedPerson -> ImprovedPerson -> Bool
  (===) x y = certificateNumber x == certificateNumber y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString ImprovedPerson where
  toString :: ImprovedPerson -> String
  toString person = (firstName person) ++ " " ++ (lastName person) ++ ", " ++ (show (age person))

-- Увеличить возраст на 1
ageUp :: ImprovedPerson -> ImprovedPerson
ageUp person = person {age = ((age person) + 1)}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: ImprovedPerson -> String -> ImprovedPerson
updateLastName person newLastName = if ((lastName person) == newLastName)
                                    then person
                                    else person {formerLastNames = (lastName person):(formerLastNames person), lastName = newLastName}

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: ImprovedPerson -> Bool
validatePerson person = ((firstName person) /= "") && ((lastName person) /= "") && ((age person) >= 0) && ( fst (certificateNumber person) /= "")

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: ImprovedPerson -> ImprovedPerson -> Bool
namesakes x y = not (x === y) && (firstName x) == (firstName y) && (lastName x) == (lastName y)

