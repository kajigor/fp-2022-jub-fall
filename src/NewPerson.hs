{-# LANGUAGE InstanceSigs #-}
module NewPerson where

import MyEq (MyEq (..))
import ToString

-- Тип данных для свидельства о рождении
data BirthCertificate = BirthCertificate
  { digit :: Int
  , letters :: String
  , number :: Int
  } deriving (Show, Eq) 

instance MyEq BirthCertificate where
    (===) :: BirthCertificate -> BirthCertificate -> Bool
    (===) x y = ((digit x) == (digit y) && (letters x) == (letters y) && (number x) == (number y))

-- Тип данных для человека
data NewPerson = NewPerson
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , idNumber :: (Int, Int)      -- Номер паспорта: состоит из серии и номера.
  , birthCertificate :: BirthCertificate -- Номер свидельства о рождении: сначала цифра, потом строка из двух латинских букв, потом номер от 0 до 999999
  }                             -- -- У детей (людей младше 14 лет) номера паспорта --- (0000, 000000)
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq NewPerson where
  (===) :: NewPerson -> NewPerson -> Bool
  (===) x y = (birthCertificate x === birthCertificate y) && (idNumber x === idNumber y)

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString NewPerson where
  toString :: NewPerson -> String
  toString person =
    (firstName person) ++ " " ++ (lastName person) ++ ", " ++ (show (age person)) 

-- Увеличить возраст на 1
ageUp :: NewPerson -> NewPerson
ageUp person =
  person{age = (age person) + 1}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: NewPerson -> String -> NewPerson
updateLastName person newLastName =
  if newLastName == (lastName person)
    then person
    else person{lastName = newLastName, formerLastNames = (lastName person):(formerLastNames person)}

-- Проверка корректности номера свидельства о рождении
validateBirthCertificate :: BirthCertificate -> Bool
validateBirthCertificate (BirthCertificate digit (symb1:symb2:[]) number) = 
    (digit >= 0 && digit <= 9) && (symb1 >= 'A' && symb1 <= 'Z') && (symb1 >= 'A' && symb2 <= 'Z') && (number >= 0 && number <= 999999)
validateBirthCertificate _ = False

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: NewPerson -> Bool
validatePerson person =
  let firstNameField = (firstName person)
      lastNameField = (lastName person) 
      ageField = (age person)
      serialIdNumber = (fst (idNumber person))
      exactIdNumber = (snd (idNumber person)) 
      birthCertificateField = (birthCertificate person) in
  firstNameField /= "" && lastNameField /= "" && ageField >= 0
  && ((ageField < 14 && serialIdNumber == 0 && exactIdNumber == 0) 
  || (ageField >= 14 && serialIdNumber > 0 && serialIdNumber <= 9999 && exactIdNumber > 0 && exactIdNumber <= 999999))
  && (validateBirthCertificate birthCertificateField)

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: NewPerson -> NewPerson -> Bool
namesakes x y =
  (firstName x) == (firstName y) && (lastName x) == (lastName y) && not (x === y)