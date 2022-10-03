{-# LANGUAGE InstanceSigs #-}
module CustomPerson where

import MyEq (MyEq (..))
import ToString

data Document = Passport Int Int | BirthCertificate String Int deriving(Show, Eq)

isChildDocument :: Document -> Bool
isChildDocument document = case document of
    Passport _ _ -> False
    BirthCertificate _ _ -> True


-- Тип данных для человека
data CustomPerson = CustomPerson
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , document :: Document     -- документ идентификации
  }                             
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq CustomPerson where
  (===) :: CustomPerson -> CustomPerson -> Bool
  (===) x y = document x == document y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString CustomPerson where
  toString :: CustomPerson -> String
  toString person = firstName person ++ " " ++ lastName person ++ ", " ++ show (age person)

-- Увеличить возраст на 1
ageUp :: CustomPerson -> CustomPerson
ageUp person = person{age = (age person) + 1}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: CustomPerson -> String -> CustomPerson
updateLastName person newLastName
  |lastName person == newLastName = person
  |otherwise = person{ lastName = newLastName, formerLastNames = lastName person : formerLastNames person}

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: CustomPerson -> Bool
validatePerson person = firstName person /= "" && lastName person /= "" && age person >= 0 && ((age person >=14 &&  not (isChildDocument (document person))) ||
 (age person <14 && isChildDocument (document person)))

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: CustomPerson -> CustomPerson -> Bool
namesakes x y = x =/= y && firstName x == firstName y && lastName x == lastName y