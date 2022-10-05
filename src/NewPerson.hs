{-# LANGUAGE InstanceSigs #-}

module NewPerson where

import MyEq (MyEq (..))
import ToString

data Passport = Passport {passNum :: (Int, Int)} deriving (Show, Eq)

data BirthCert = BirthCert {bcNum :: (String, Int)} deriving (Show, Eq)

data OtherDoc = OtherDoc {docType :: String, docInfo :: String} deriving (Show, Eq)

data IdDocument = Passport_ Passport | BirthCert_ BirthCert | OtherDoc_ OtherDoc deriving (Show, Eq)

-- I don't print doc details, but to derive NewPerson from `Show` I had to add `derive` to IdDocuments and so to all types of documents (Passport, etc). How can I avoid that?

instance ToString IdDocument where
  toString :: IdDocument -> String
  toString _ = "TODO"

validateDoc :: IdDocument -> Int -> Bool -- id, age -> bool
validateDoc (Passport_ p) age =
  age >= 14
    && fst (passNum p) >= 0
    && fst (passNum p) <= 9999
    && snd (passNum p) >= 0
    && snd (passNum p) <= 999999
validateDoc (BirthCert_ bc) age =
  age < 14
    && length (fst (bcNum bc)) == 3
    && snd (bcNum bc) >= 0
    && snd (bcNum bc) <= 999999
validateDoc (OtherDoc_ _) _ = True

-- Passport = (1234, 567890)
-- BirthCert = ("ICP", 123456)
-- OtherDoc = ("Any Doc", "123456")

instance MyEq IdDocument where
  (===) :: IdDocument -> IdDocument -> Bool
  (===) (Passport_ x) (Passport_ y) = passNum x == passNum y
  (===) (BirthCert_ x) (BirthCert_ y) = bcNum x == bcNum y
  (===) (OtherDoc_ x) (OtherDoc_ y) = docType x == docType y && docInfo x == docInfo y
  (===) _ _ = False

-- Тип данных для человека
data NewPerson = NewPerson
  { firstName :: String, -- Имя, должно быть непустым
    lastName :: String, -- Фамилия, должна быть непустой
    formerLastNames :: [String], -- Предыдущие фамилии, если фамилия менялась
    age :: Int, -- Возраст, должен быть неотрицательным
    idDoc :: IdDocument
  }
  deriving (Show, Eq)

-- У разных людей разные номера паспортов
instance MyEq NewPerson where
  (===) :: NewPerson -> NewPerson -> Bool
  (===) x y = idDoc x === idDoc y

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString NewPerson where
  toString :: NewPerson -> String
  toString person = firstName person ++ " " ++ lastName person ++ ", " ++ toString (age person)

-- Увеличить возраст на 1
ageUp :: NewPerson -> NewPerson
ageUp person = person {age = age person + 1}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: NewPerson -> String -> NewPerson
updateLastName person newLastName
  | newLastName /= lastName person = person {lastName = newLastName, formerLastNames = lastName person : formerLastNames person}
  | otherwise = person

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: NewPerson -> Bool
validatePerson person
  | firstName person /= ""
      && lastName person /= ""
      && age person >= 0
      && validateDoc (idDoc person) (age person) =
    True
  | otherwise = False

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: NewPerson -> NewPerson -> Bool
namesakes x y
  | x =/= y
      && firstName x == firstName y
      && lastName x == lastName y =
    True
  | otherwise = False
