{-# LANGUAGE InstanceSigs #-}
module PersonUpdated where

import MyEq (MyEq (..))
import ToString

-- Тип данных для человека
data PersonUpdated = PersonUpdated
  { firstName :: String                 -- Имя, должно быть непустым
  , lastName :: String                  -- Фамилия, должна быть непустой
  , formerLastNames :: [String]         -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                          -- Возраст, должен быть неотрицательным
  , idNumber :: (Int, Int)              -- Номер паспорта: состоит из серии и номера.
                                        -- -- У детей (людей младше 14 лет) номера паспорта --- (0000, 000000)
  , birthCertificate :: (String, Int)   -- Номер свидетельства о рождении: римская цифры, 2 буквы, 6 цифр
  }
  deriving (Show, Eq)

-- У разных людей разные номера свидетельств о рождении
instance MyEq PersonUpdated where
  (===) :: PersonUpdated -> PersonUpdated -> Bool
  (===) person1 person2 = birthCertificate person1 === birthCertificate person2

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString PersonUpdated where
  toString :: PersonUpdated -> String
  toString person = firstName person ++ " " ++ lastName person ++ ", " ++ toString(age person)

-- Увеличить возраст на 1
ageUp :: PersonUpdated -> PersonUpdated
ageUp person = person {age = age person + 1}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: PersonUpdated -> String -> PersonUpdated
updateLastName person newLastName | newLastName /= lastName person = person {formerLastNames = [lastName person] ++ formerLastNames person, lastName = newLastName}
                                  | otherwise = person

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: PersonUpdated -> Bool
validatePerson person | null (firstName person) || null (lastName person) || age person < 0
                      || ((idNumber person /= (0000, 000000) || null (fst (birthCertificate person))) && age person < 14) = False
                      | otherwise = True

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: PersonUpdated -> PersonUpdated -> Bool
namesakes person1 person2 = not (person1 === person2) && firstName person1 == firstName person2 && lastName person1 == lastName person2
