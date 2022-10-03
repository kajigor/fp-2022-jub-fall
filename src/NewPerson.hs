{-# LANGUAGE InstanceSigs #-}

module NewPerson where

import MyEq (MyEq (..))
import ToString

data Adult = Adult
  { aFirstName :: String, -- Имя, должно быть непустым
    aLastName :: String, -- Фderiving (Show, Eq)амилия, должна быть непустой
    aFormerLastNames :: [String], -- Предыдущие фамилии, если фамилия менялась
    aAge :: Int, -- Возраст, должен быть неотрицательным
    aIdNumber :: (Int, Int) -- Номер паспорта: состоит из серии и номера.
  }
  deriving (Show, Eq)

data Child = Child
  { cFirstName :: String, -- Имя, должно быть непустым
    cLastName :: String, -- Фамилия, должна быть непустой
    cFormerLastNames :: [String], -- Предыдущие фамилии, если фамилия менялась
    cAge :: Int -- Возраст, должен быть неотрицательным
  }
  deriving (Show, Eq)

data NewPerson
  = AdultPerson Adult
  | ChildPerson Child
  deriving (Show, Eq)

instance MyEq NewPerson where
  (===) :: NewPerson -> NewPerson -> Bool
  (===) (AdultPerson a) (AdultPerson b) = aIdNumber a == aIdNumber b
  (===) (ChildPerson a) (ChildPerson b) = cFirstName a == cFirstName b && cLastName a == cLastName b && cAge a == cAge b
  (===) _ _ = False

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString NewPerson where
  toString :: NewPerson -> String
  toString (AdultPerson person) = aFirstName person ++ " " ++ aLastName person ++ ", " ++ toString (aAge person)
  toString (ChildPerson person) = cFirstName person ++ " " ++ cLastName person ++ ", " ++ toString (cAge person)

-- Увеличить возраст на 1
ageUp :: NewPerson -> NewPerson
ageUp (AdultPerson person) =
  AdultPerson person {aAge = aAge person + 1}
ageUp (ChildPerson person)
  | cAge person == 13 =
    AdultPerson
      Adult
        { aFirstName = cLastName person,
          aLastName = cLastName person,
          aFormerLastNames = cFormerLastNames person,
          aAge = 14,
          aIdNumber = undefined
        }
  | otherwise = ChildPerson person {cAge = cAge person + 1}

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames
updateLastName :: NewPerson -> String -> NewPerson
updateLastName (ChildPerson person) newLastName
  | cLastName person == newLastName = ChildPerson person
  | otherwise = ChildPerson person {cLastName = newLastName, cFormerLastNames = cLastName person : cFormerLastNames person}
updateLastName (AdultPerson person) newLastName
  | aLastName person == newLastName = AdultPerson person
  | otherwise = AdultPerson person {aLastName = newLastName, aFormerLastNames = aLastName person : aFormerLastNames person}

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: NewPerson -> Bool
validatePerson (AdultPerson a) = aFirstName a /= [] && aLastName a /= [] && aAge a >= 0 && aAge a >= 14 && aIdNumber a /= (0000, 000000)
validatePerson (ChildPerson c) = cFirstName c /= [] && cLastName c /= [] && cAge c >= 0 && cAge c < 14

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями
namesakes :: NewPerson -> NewPerson -> Bool
namesakes (AdultPerson a) (ChildPerson b) = aFirstName a == cFirstName b && aLastName a == cLastName b
namesakes (AdultPerson a) (AdultPerson b) = aFirstName a == aFirstName b && aLastName a == aLastName b && aIdNumber a /= aIdNumber b
namesakes (ChildPerson a) (AdultPerson b) = namesakes (AdultPerson b) (ChildPerson a)
namesakes (ChildPerson a) (ChildPerson b) = cFirstName a == cFirstName b && cLastName a == cLastName b && cAge a /= cAge b
