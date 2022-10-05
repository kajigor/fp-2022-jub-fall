{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
module Human where

import MyEq (MyEq (..))
import ToString

import Data.List

data DocumentID = Passport (Int, Int) | -- Формат ID пасспорта: (xxxx, xxxxxx) 
                  BirthCertificate Int  -- Формат ID свидетельства о рождении: xxxxxxxxxx    (Не уверен, что это правда, но и не важно)
                  deriving (Show, Eq)   --                                     <---10--->


-- Тип данных для человека
data Human = Human
  { firstName :: String         -- Имя, должно быть непустым
  , lastName :: String          -- Фамилия, должна быть непустой
  , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
  , age :: Int                  -- Возраст, должен быть неотрицательным
  , documentID :: DocumentID    -- Номер паспорта или свидетельства о рождении: смотреть DocumentID
  }
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq Human where
  (===) :: Human -> Human -> Bool
  (===) x y = documentID x == documentID y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
-- Иисус Христос, 2022
instance ToString Human where
  toString :: Human -> String
  toString (Human { .. }) = firstName ++ " " ++ lastName ++ ", " ++ (show age)

-- Увеличить возраст на 1
ageUp :: Human -> Human
ageUp (Human { .. }) = Human firstName lastName formerLastNames (age + 1) documentID

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: Human -> String -> Human
updateLastName (Human { .. }) newLastName | lastName /= newLastName && (not $ isInfixOf [lastName] formerLastNames) = Human firstName newLastName (lastName : formerLastNames) age documentID
                                          | otherwise = Human firstName newLastName formerLastNames age documentID 

-- Проверки на корректность (указаны в комментариях к типу данных)
validateHuman :: Human -> Bool
validateHuman Human { .. } = (isCorrectFirstName firstName) && 
                             (isCorrectLastName lastName) && 
                             (isCorrectFormerNames formerLastNames) && 
                             (isCorrectAge age) && 
                             (isCorrectdocumentID documentID) &&
                             (isRightDocument age documentID)
  where
    isCorrectFirstName :: String -> Bool
    isCorrectFirstName fName = not $ null fName

    isCorrectLastName :: String -> Bool
    isCorrectLastName lName = not $ null lName

    isCorrectFormerNames :: [String] -> Bool
    isCorrectFormerNames = foldr f True
      where
        f :: String -> Bool -> Bool
        f str acc = acc && (isCorrectLastName str)

    isCorrectAge :: Int -> Bool
    isCorrectAge age = age >= 0

    isCorrectdocumentID :: DocumentID -> Bool 
    isCorrectdocumentID (Passport (x, y)) = (0 < x && x < 10^4) && (0 < y && y < 10^6) 
    isCorrectdocumentID (BirthCertificate x) = 0 < x && x < 10^10

    isRightDocument age (Passport _) = age > 14
    isRightDocument age (BirthCertificate _) = age <= 14
                                         

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: Human -> Human -> Bool
namesakes p1 p2 = (firstName p1, lastName p1) == (firstName p2, lastName p2) && p1 =/= p2 
