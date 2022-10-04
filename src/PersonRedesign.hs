{-# LANGUAGE InstanceSigs #-}
module PersonRedesign where

import MyEq (MyEq (..))
import ToString

data Id = RussianPassport (Int, Int) | RussianBirthCertificate (String, Int) deriving (Show, Eq)

validateId :: Id -> Bool
validateId (RussianPassport (series, number)) = (0 <= series) && (series <= 9999)
validateId (RussianBirthCertificate (series, number)) = (0 <= number) && (number <= 999999)

-- Тип данных для человека
data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , id' :: Id           -- Информация о документе, удостоверяющем личность
  }                             -- У детей (людей младше 14 лет) id обязательно RussianBirthCertificate,
                                -- у остальных — обязательно паспорт
  deriving (Show, Eq)

instance MyEq Id where
    (===) :: Id -> Id -> Bool
    (===) (RussianPassport number1) (RussianPassport number2) = number1 === number2
    (===) (RussianBirthCertificate number1) (RussianBirthCertificate number2) = number1 === number2
    (===) _ _ = False

-- У разных людей разные preferredId
instance MyEq Person where
  (===) :: Person -> Person -> Bool
  (===) x y = id' x === id' y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString Person where
  toString :: Person -> String
  toString person = concat [firstName person, " ", lastName person, ", ", toString (age person)]

-- Увеличить возраст на 1
ageUp :: Person -> Person
ageUp person = person { age = age person + 1 }

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: Person -> String -> Person
updateLastName person newLastName
  | oldLastName =/= newLastName = person { lastName = newLastName, formerLastNames = oldLastName : formerLastNames person }
  | otherwise = person
  where oldLastName = lastName person

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: Person -> Bool
validatePerson person = 
  not (null (firstName person))     &&
  not (null (lastName person))      &&
  validateId (id' person)   && 
  case id' person of
    (RussianBirthCertificate _) -> age person < 14
    (RussianPassport _) -> age person >= 14

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: Person -> Person -> Bool
namesakes x y = (getPersonName x === getPersonName y) && (x =/= y)
  where getPersonName person = (firstName person, lastName person)