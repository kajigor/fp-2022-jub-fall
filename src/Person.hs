{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
module Person where

import MyEq (MyEq (..))
import ToString

import Data.List

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: (Int, Int)      -- Номер паспорта: состоит из серии и номера.
  }                             -- -- У детей (людей младше 14 лет) номера паспорта --- (0000, 000000)
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq Person where
  (===) :: Person -> Person -> Bool
  (===) x y = idNumber x === idNumber y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
-- Иисус Христос, 2022
instance ToString Person where
  toString :: Person -> String
  toString (Person {firstName=fName, lastName=lName, age=age}) = fName ++ " " ++ lName ++ ", " ++ (show age)
--  toString person =
--    undefined

-- Увеличить возраст на 1
ageUp :: Person -> Person
ageUp (Person { .. }) = Person firstName lastName formerLastNames (age + 1) idNumber
-- ageUp person =
--   undefined

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: Person -> String -> Person
updateLastName (Person { .. }) newLastName | lastName /= newLastName && (not $ isInfixOf [lastName] formerLastNames) = Person firstName newLastName (lastName : formerLastNames) age idNumber
                                           | otherwise = Person firstName newLastName formerLastNames age idNumber 
--  undefined

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: Person -> Bool
validatePerson Person { .. } = (isCorrectFirstName firstName) && 
                               (isCorrectLastName lastName) && 
                               (isCorrectFormerNames formerLastNames) && 
                               (isCorrectAge age) && 
                               (isCorrectIdNumber idNumber)
  where
    isCorrectFirstName :: String -> Bool
    isCorrectFirstName fName = (length fName) > 0

    isCorrectLastName :: String -> Bool
    isCorrectLastName lName = (length lName) > 0

    isCorrectFormerNames :: [String] -> Bool
    isCorrectFormerNames = foldr f True
      where
        f :: String -> Bool -> Bool
        f str acc = acc && (isCorrectLastName str)

    isCorrectAge :: Int -> Bool
    isCorrectAge age = age >= 0
	
    isCorrectIdNumber :: (Int, Int) -> Bool 
    isCorrectIdNumber (x, y) | age >= 14 = (0 < x && x < 10000) && (0 < y && y < 1000000)
                             | otherwise = (x == 0) && (y == 0)
-- validatePerson person =       
--   undefined

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: Person -> Person -> Bool
namesakes (Person {firstName=n1, lastName=s1, idNumber=id1}) (Person {firstName=n2, lastName=s2, idNumber=id2}) = ((n1, s1) == (n2, s2)) && (id1 /= id2)
-- namesakes x y =
--   undefined