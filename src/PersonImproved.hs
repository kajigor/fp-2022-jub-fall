{-# LANGUAGE InstanceSigs #-}
module PersonImproved where

import MyEq (MyEq (..))
import ToString

-- Тип данных для человека
data PersonImproved = PersonImproved
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: (Int, Int)      -- Номер паспорта: состоит из серии и номера.
  , birthCertificateNumber :: (Int, String, Int) 
                                -- Номер свидетельства о рождении: состоит из серии (число и буквы через дефис) и самого номера. 
                                -- У взрослых данные о нём могут остутстовать и быть равны (0, "", 0)
  }                             -- У детей (людей младше 14 лет) номера паспорта --- (0000, 000000)
  deriving (Show, Eq)

-- У разных людей разные номера паспортов, если они взрослые, и разные номера свидетельства рождения
instance MyEq PersonImproved where
  (===) :: PersonImproved -> PersonImproved -> Bool
  (===) x y = (idNumber x === idNumber y && idNumber x /= (0, 0)) || (birthCertificateNumber x == birthCertificateNumber y)

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString PersonImproved where
  toString :: PersonImproved -> String
  toString person = (firstName person) ++ " " ++ (lastName person) ++ ", " ++ (toString (age person))

-- Увеличить возраст на 1
ageUp :: PersonImproved -> PersonImproved
ageUp person = person {age = age person + 1}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: PersonImproved -> String -> PersonImproved
updateLastName person newLastName = if (newLastName == lastName person) then person else person {lastName = newLastName, formerLastNames = (lastName person) : (formerLastNames person)}

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: PersonImproved -> Bool
validatePerson person = (firstName person) /= "" && (lastName person) /= "" && (age person) >= 0 && (age person >= 14 || idNumber person == (0, 0) && birthCertificateNumber person /= (0, "", 0))

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: PersonImproved -> PersonImproved -> Bool
namesakes x y = firstName x == firstName y && lastName x == lastName y && not (x === y)