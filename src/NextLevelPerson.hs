{-# LANGUAGE InstanceSigs #-}

module NextLevelPerson
  ( NextLevelPerson (..),
    DocumentHolder (..),
    Document (..),
    addPassport,
    addBirthCertificate,
    addDocument,
    getDocument,
    ageUp,
    updateLastName,
    validatePerson,
    namesakes,
  )
where

import Data.List (find)
import Data.Maybe (fromJust, isJust)
import MyEq (MyEq (..))
import ToString (ToString (..))

data Document
  = Passport (Int, Int) -- Номер паспорта состоит из серии и номера, выдается в 14 лет.
  | BirthCertificate Int -- Номер свидетельства о рождении.
  deriving (Show, Eq)

instance MyEq Document where
  (===) :: Document -> Document -> Bool
  (===) (BirthCertificate x) (BirthCertificate y) = x == y
  (===) (Passport (x1, x2)) (Passport (y1, y2)) = x1 == x2 && y1 == y2
  (===) _ _ = False

data DocumentHolder = DocumentHolder
  { passports :: [Document], -- выпущенные паспорта
    birthCertificates :: [Document] -- выпущенные свидетельства о рождении
  }

-- добавляет новый паспорт
addPassport :: [Document] -> [Document]
addPassport (Passport (x, y) : xs) = Passport (1 + x, 1 + y) : Passport (x, y) : xs
addPassport [] = [Passport (0001, 000001)]
addPassport _ = undefined

-- добавляет новое свидетельство о рождении
addBirthCertificate :: [Document] -> [Document]
addBirthCertificate (BirthCertificate x : xs) = BirthCertificate (1 + x) : BirthCertificate x : xs
addBirthCertificate [] = [BirthCertificate 000001]
addBirthCertificate _ = undefined

-- добавляет новый документ. Тип зависимосит от возраста
addDocument :: Int -> DocumentHolder -> DocumentHolder
addDocument personAge documents =
  if personAge > 14
    then DocumentHolder (addPassport (passports documents)) (birthCertificates documents)
    else DocumentHolder (passports documents) (addBirthCertificate (birthCertificates documents))

-- возвращает последний добавленный документ
getDocument :: Int -> DocumentHolder -> Document
getDocument personAge documents = if personAge < 14 then head (birthCertificates documents) else head (passports documents)

-- Тип данных для человека
data NextLevelPerson = NextLevelPerson
  { firstName :: String, -- Имя, должно быть непустым
    lastName :: String, -- Фамилия, должна быть непустой
    formerLastNames :: [String], -- Предыдущие фамилии, если фамилия менялась
    age :: Int, -- Возраст, должен быть неотрицательным
    document :: Maybe Document, -- Документ, по которому можно однозначно идентифицировать человека
    formerDocuments :: [Document] -- Предыдущие документы
  }
  deriving (Show, Eq)

-- разворачивает Maybe перед сравнением
myEquals :: Maybe Document -> Maybe Document -> Bool
myEquals Nothing Nothing = False -- считаем, что если документов у людей нет, то это разные люди
myEquals _ Nothing = False
myEquals Nothing _ = False
myEquals x y = fromJust x == fromJust y

-- У разных людей разные номера паспортов или свидетельств о рождении
instance MyEq NextLevelPerson where
  (===) :: NextLevelPerson -> NextLevelPerson -> Bool
  (===) x y = myEquals (document x) (document y)

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString NextLevelPerson where
  toString :: NextLevelPerson -> String
  toString person = firstName person ++ " " ++ lastName person ++ ", " ++ show (age person)

-- Увеличить возраст на 1, если человеку исполняется 14, то выпустить паспорт
ageUp :: NextLevelPerson -> DocumentHolder -> NextLevelPerson
ageUp person documents =
  if nextAge == 14
    then
      NextLevelPerson
        { firstName = firstName person,
          lastName = lastName person,
          formerLastNames = formerLastNames person,
          age = 1 + age person,
          document = Just (getDocument nextAge documents),
          formerDocuments = formerDocuments person
        }
    else
      NextLevelPerson
        { firstName = firstName person,
          lastName = lastName person,
          formerLastNames = formerLastNames person,
          age = 1 + age person,
          document = document person,
          formerDocuments = formerDocuments person
        }
  where
    nextAge = 1 + age person

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
-- Также выпускается новый документ
updateLastName :: NextLevelPerson -> String -> DocumentHolder -> NextLevelPerson
updateLastName person newLastName documents =
  if newLastName == lastName person
    then person
    else
      NextLevelPerson
        { firstName = firstName person,
          lastName = newLastName,
          formerLastNames = lastName person : formerLastNames person,
          age = age person,
          document = Just (getDocument (age person) documents),
          formerDocuments = fromJust (document person) : formerDocuments person
        }

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: NextLevelPerson -> DocumentHolder -> Bool
validatePerson person documents
  | firstName person == "" = False
  | lastName person == "" = False
  | age person < 0 = False
  | not (validateMaybeDocument (age person) (document person)) = False
  | not (isExistingMaybeDocument (age person) (document person) documents) = False
  | otherwise = True

-- разворачивает Maybe чтобы провалидировать документ
validateMaybeDocument :: Int -> Maybe Document -> Bool
validateMaybeDocument _ Nothing = False
validateMaybeDocument personAge (Just personDocument) = validateDocument personAge personDocument

tooYoungToHavePassport :: Int -> Bool
tooYoungToHavePassport personAge = personAge < 14

validateDocument :: Int -> Document -> Bool
validateDocument personAge personDocument =
  if tooYoungToHavePassport personAge
    then validateBirthcertificate personDocument
    else validatePassport personDocument

-- проверяет свидетельство о рождении
validateBirthcertificate :: Document -> Bool
validateBirthcertificate (BirthCertificate x) = x > 0
validateBirthcertificate _ = False

-- проверяет паспорт
validatePassport :: Document -> Bool
validatePassport (Passport (x, y)) = x > 0 && y > 0
validatePassport _ = False

-- разворачивает Maybe чтобы проверить что документ есть в списке выданных
isExistingMaybeDocument :: Int -> Maybe Document -> DocumentHolder -> Bool
isExistingMaybeDocument _ Nothing _ = False
isExistingMaybeDocument personAge (Just personDocument) documents = isExistingDocument personAge personDocument documents

-- првоеряет наличие документа в списке выданных
isExistingDocument :: Int -> Document -> DocumentHolder -> Bool
isExistingDocument personAge personDocument documents =
  if tooYoungToHavePassport personAge
    then isJust (find (== personDocument) (birthCertificates documents))
    else isJust (find (== personDocument) (passports documents))

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди (у них разные документы) с одинаковыми именами и фамилиями
namesakes :: NextLevelPerson -> NextLevelPerson -> Bool
namesakes x y
  | firstName x == firstName y && lastName x == lastName y && document x /= document y = True
  | otherwise = False