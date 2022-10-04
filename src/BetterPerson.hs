{-# LANGUAGE InstanceSigs #-}
module BetterPerson where

import MyEq (MyEq (..))
import ToString

-- Тип данных для человека
data Person = Child {
    firstName :: String
  , lastName :: String
  , formerLastNames :: [String]
  , age :: Int
  , birthCertificate :: (Int, Int)
  } | Adult {
    firstName :: String
  , lastName :: String
  , formerLastNames :: [String]
  , age :: Int
  , passport :: (Int, Int)
  }
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq Person where
  (===) :: Person -> Person -> Bool
  (===) Child { birthCertificate = x } Child { birthCertificate = y } = x === y
  (===) Adult { passport = x } Adult { passport = y } = x === y
  (===) _ _ = False

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString Person where
  toString :: Person -> String
  toString person =
    firstName person ++ " " ++ lastName person ++ ", " ++ show (age person)

-- Увеличить возраст на 1
ageUp :: Person -> Person
ageUp person =
  person { age = age person + 1 }

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: Person -> String -> Person
updateLastName person newLastName =
  if lastName person /= newLastName then
    person { lastName = newLastName, formerLastNames = lastName person : formerLastNames person }
  else
    person

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: Person -> Bool
validatePerson person =
  not (null (firstName person)) && not (null (lastName person)) && age person >= 0

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: Person -> Person -> Bool
namesakes x y =
  firstName x == firstName y && lastName x == lastName y && x =/= y
