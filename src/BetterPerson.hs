{-# LANGUAGE InstanceSigs #-}

module BetterPerson where

import           MyEq     (MyEq (..))
import           ToString

-- Тип данных для удостоверения личности

-- Считаем, что у ребёнка есть свой аналог паспорта — свидетельство о рождении.
-- Считаем также, что у взрослого человека свидетельство никуда не исчезает.
-- По этой логике, взрослый человек и ребёнок, у которых совпадают номер свидетельства о рождении, являются одним человеком.
data PersonalId
  = Passport
      { passportNumber         :: (Int, Int)
      , birthCertificateNumber :: (String, String, Int)
      }
  | BirthCertificate
      { certificateNumber :: (String, String, Int) -- 2 заглавные латинские буквы, 2 заглавные буквы кирилицы, 6 цифр
      }

-- Тип данных для человека
data BetterPerson =
  BetterPerson
    { firstName       :: String -- Имя, должно быть непустым
    , lastName        :: String -- Фамилия, должна быть непустой
    , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
    , age             :: Int -- Возраст, должен быть неотрицательным
    , personalId      :: PersonalId -- Номер паспорта: состоит из серии и номера.
    }
  deriving (Show, Eq)

-- У разных людей разные номера документов
instance Eq PersonalId where
  (==) :: PersonalId -> PersonalId -> Bool
  (==) (Passport x1 y1) (Passport x2 y2) = x1 == x2 && y1 == y2
  (==) (BirthCertificate x) (BirthCertificate y) = x == y
  (==) (Passport _ xCertificateNumber) (BirthCertificate yCertificateNumber) =
    xCertificateNumber == yCertificateNumber
  (==) (BirthCertificate yCertificateNumber) (Passport _ xCertificateNumber) =
    xCertificateNumber == yCertificateNumber

instance Show PersonalId where
  show :: PersonalId -> String
  show (Passport a b)       = show a ++ show b
  show (BirthCertificate x) = show x

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString BetterPerson where
  toString :: BetterPerson -> String
  toString BetterPerson { firstName = _firstName
                        , lastName = _lastName
                        , age = _age
                        } =
    _firstName ++ " " ++ _lastName ++ ", " ++ toString _age

-- Увеличить возраст на 1
ageUp :: BetterPerson -> BetterPerson
ageUp person = person {age = 1 + age person}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: BetterPerson -> String -> BetterPerson
updateLastName person newLastName =
  if newLastName == lastName person
    then person
    else person
           { formerLastNames = lastName person : formerLastNames person
           , lastName = newLastName
           }

-- Проверки на корректность (указаны в комментариях к типу данных)
cyrillicLetters :: [Char]
cyrillicLetters = ['А' .. 'Я']

latinLetters :: [Char]
latinLetters = ['I', 'X', 'V']

-- Проверяем, что все буквы из line содержатся в charSet
checkAllLettersInSet :: String -> [Char] -> Bool
checkAllLettersInSet line charSet = all (`elem` charSet) line

-- Отдельно валидируем свидетельство о рождении для удобства
validateBirthCertificate :: PersonalId -> Bool
validateBirthCertificate (BirthCertificate (latin_letters, cyrillic_letters, number)) =
  checkAllLettersInSet latin_letters latinLetters &&
  length latin_letters == 2 &&
  checkAllLettersInSet cyrillic_letters cyrillicLetters &&
  length cyrillic_letters == 2 && 0 <= number && number < 10000000
validateBirthCertificate _ = False

validateIdFormat :: Int -> PersonalId -> Bool
validateIdFormat _age (Passport (id_series, id_number) _certificateNumber) =
  _age >= 14 &&
  validateBirthCertificate (BirthCertificate _certificateNumber) &&
  0 <= id_series && id_series < 10000 && 0 <= id_number && id_number < 1000000
validateIdFormat _age (BirthCertificate _certificateNumber) =
  _age < 14 && validateBirthCertificate (BirthCertificate _certificateNumber)

validatePerson :: BetterPerson -> Bool
validatePerson person =
  firstName person /= "" &&
  lastName person /= "" &&
  age person >= 0 && validateIdFormat (age person) (personalId person)

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: BetterPerson -> BetterPerson -> Bool
namesakes x y =
  firstName x == firstName y &&
  lastName x == lastName y && personalId x /= personalId y
