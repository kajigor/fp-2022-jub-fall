{-# LANGUAGE InstanceSigs #-}
module NewPerson where

import MyEq (MyEq (..))
import ToString

-- Тип данных для человека
data NewPerson = NewPerson
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , passport :: (Int, Int)      -- Номер паспорта: состоит из серии и номера.
                                -- -- У детей (людей младше 14 лет) номера паспорта --- (0000, 000000)
  , birthId :: (String, Int)    -- Номер свидетельства о рождении: состоит из серии - 2 буквы, и номера - 6 цифр
  }
  deriving (Show, Eq)

-- У разных людей разные номера паспортов, 
-- -- если же у одного из людей нет паспорта, сравниваются номера свидетельств о рождении
instance MyEq NewPerson where
  (===) :: NewPerson -> NewPerson -> Bool
  (===) x y 
    | passport x === (0, 0) || passport y === (0, 0) = birthId x === birthId y
    | otherwise = passport x === passport y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString NewPerson where
  toString :: NewPerson -> String
  toString person = 
    firstName person ++ " " ++ lastName person ++ ", " ++ toString (age person)

-- Увеличить возраст на 1
ageUp :: NewPerson -> NewPerson
ageUp person =
  person { age = age person + 1 }

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: NewPerson -> String -> NewPerson
updateLastName person newLastName
  | newLastName == lastName person = person
  | otherwise = person { lastName = newLastName, formerLastNames = lastName person : formerLastNames person}

validatePassport :: (Int, Int) -> Int -> Bool
validatePassport passport age =
    passport <= (9999, 999999) && passport >= (0, 0) && (age >= 14 || passport == (0, 0))

validateBirthId :: (String, Int) -> Bool
validateBirthId birthId = length (fst birthId) == 2 && snd birthId >= 0 && snd birthId <= 999999

-- Проверки на корректность (указаны в комментариях к типу данных)
validateNewPerson :: NewPerson -> Bool
validateNewPerson person =
  firstName person /= "" && lastName person /= "" && age person >= 0 && validatePassport (passport person) (age person) && validateBirthId (birthId person)

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: NewPerson -> NewPerson -> Bool
namesakes x y =
  x =/= y && (firstName x, lastName x) == (firstName y, lastName y)