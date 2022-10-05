{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
module MyPerson where

import MyEq (MyEq (..))
import ToString

data Document = Id { inumber :: (Int, Int) } | BirthCert { bnumber :: Int } deriving (Show, Eq)

instance MyEq Document where
  (===) :: Document -> Document -> Bool
  (===) (Id x) (Id y) = x === y
  (===) (BirthCert x) (BirthCert y) = x === y
  (===) _ _ = False

isId :: Document -> Bool
isId (Id x) = True
isId _ = False

-- Тип данных для человека
data MyPerson = MyPerson
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , document :: Document      -- Номер паспорта: состоит из серии и номера.
  }                             -- -- У детей (людей младше 14 лет) номера паспорта --- (0000, 000000)
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq MyPerson where
  (===) :: MyPerson -> MyPerson -> Bool
  (===) x y = document x === document y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString MyPerson where
  toString :: MyPerson -> String
  toString (MyPerson {firstName, lastName, age}) = firstName ++ " " ++ lastName ++ ", " ++ show age 

-- Увеличить возраст на 1
ageUp :: MyPerson -> MyPerson
ageUp person = person {age = age person + 1}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: MyPerson -> String -> MyPerson
updateLastName person newLastName | lastName person /= newLastName = person{formerLastNames = lastName person : formerLastNames person, lastName = newLastName}
                                  | otherwise = person

-- Проверки на корректность (указаны в комментариях к типу данных)
validateMyPerson :: MyPerson -> Bool
validateMyPerson (MyPerson {firstName}) | firstName == [] = False
validateMyPerson (MyPerson {lastName}) | lastName == [] = False
validateMyPerson (MyPerson {age}) | age < 0 = False
validateMyPerson (MyPerson {document, age}) | age >= 14 && not (isId document) = False
validateMyPerson (MyPerson {document, age}) | age < 14 && isId document = False
validateMyPerson _ = True

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: MyPerson -> MyPerson -> Bool
namesakes x y | x === y = False
namesakes x y | firstName x == firstName y && lastName x == lastName y = True
namesakes _ _ = False