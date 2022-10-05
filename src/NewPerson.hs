{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ImplicitParams #-}
module NewPerson where

import MyEq (MyEq (..))
import ToString
import System.Random

-- Тип данных для человека
data Document = Passport (Int, Int) | BirthCertificate (String, Int)
    deriving (Show, Eq)

data Person = Person
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , documentNumber :: Document
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  
  }                             -- -- У детей (людей младше 14 лет) номера паспорта --- (0000, 000000)
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq Document where
  (===) :: Document -> Document -> Bool
  (===) (Passport x) (Passport y) = x == y
  (===) (BirthCertificate x) (BirthCertificate y) = x == y
  (===) _ _ = False

instance MyEq Person where
  (===) :: Person -> Person -> Bool
  (===) x y = documentNumber x == documentNumber y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString Person where
  toString :: Person -> String
  toString person =
    firstName person ++ " " ++ lastName person ++ ", " ++ (toString $ age person)

-- Увеличить возраст на 1
ageUp :: Person -> Person
ageUp person | age person /= 13 = let ?newDocument = documentNumber person in ageUpWithIdUpdate person
             | otherwise = let ?newDocument = Passport ( (fst $ randomR (1000, 9999) (mkStdGen 1)), ( fst $ randomR (100000, 999999) (mkStdGen 1)) ) in ageUpWithIdUpdate person

ageUpWithIdUpdate :: (?newDocument :: Document) => Person -> Person
ageUpWithIdUpdate person = person { age = age person + 1, documentNumber = ?newDocument} 

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: Person -> String -> Person
updateLastName person newLastName 
  | newLastName == lastName person = person
  | otherwise = person {formerLastNames = lastName person : formerLastNames person, lastName = newLastName}

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: Person -> Bool
validatePerson person | age person >= 14 =
  firstName person /= "" &&
  lastName person /= "" &&
  case documentNumber person of
    Passport _ -> True
    otherwise -> False

validatePerson person | age person < 14 =
  firstName person /= "" &&
  lastName person /= "" &&
  age person > 0 && 
  case documentNumber person of
    BirthCertificate _ -> True
    otherwise -> False

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: Person -> Person -> Bool
namesakes x y =
  firstName x == firstName y && lastName x == lastName y && documentNumber x /= documentNumber y