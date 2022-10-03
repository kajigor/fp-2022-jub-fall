{-# LANGUAGE InstanceSigs #-}
module NewPerson where

import MyEq (MyEq (..))
import ToString

data IdNumber = 
  Passport {series_p :: Int, number_p :: Int} |
  BirthSertificate {series_s :: String, number_s :: Int}
  deriving (Show, Eq)

-- Тип данных для человека
data NewPerson = NewPerson
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным   
  , idNumber :: IdNumber   
  }                            
  deriving (Show, Eq)

instance MyEq IdNumber where 
  (===) :: IdNumber -> IdNumber -> Bool
  (===) (Passport s1 n1) (Passport s2 n2) = s1 == s2 && n1 == n2
  (===) (BirthSertificate s1 n1) (BirthSertificate s2 n2) = s1 == s2 && n1 == n2
  (===) _ _ = False

-- У разных людей разные номера паспортов
instance MyEq NewPerson where
  (===) :: NewPerson -> NewPerson -> Bool
  (===) x y = idNumber x === idNumber y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString NewPerson where
  toString :: NewPerson -> String
  toString person =
    firstName person ++ " " ++ lastName person ++ ", " ++ (show (age person))

-- Увеличить возраст на 1
ageUp :: NewPerson -> NewPerson
ageUp person =
  person {age = age person + 1}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: NewPerson -> String -> NewPerson
updateLastName person newLastName =
  person {formerLastNames = newLastNames, lastName = newLastName}
    where newLastNames | newLastName == lastName person = formerLastNames person
                       | otherwise = lastName person : formerLastNames person 

isPassport :: IdNumber -> Bool
isPassport (Passport s1 n1) = True
isPassport _ = False

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: NewPerson -> Bool
validatePerson person =
  not(null (firstName person)) && not (null (lastName person)) && age person >= 0 && (age person >= 14 && isPassport (idNumber person) ||
  age person < 14 && not (isPassport (idNumber person)))

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: NewPerson -> NewPerson -> Bool
namesakes x y =
  (firstName x == firstName y) && (lastName x == lastName y) && not(x === y)