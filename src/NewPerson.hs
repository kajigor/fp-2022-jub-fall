{-# LANGUAGE InstanceSigs #-}

module NewPerson where

import MyEq (MyEq (..))
import ToString

data Child = Child
  { firstName :: String, -- Имя, должно быть непустым
    lastName :: String, -- Фамилия, должна быть непустой
    formerLastNames :: [String], -- Предыдущие фамилии, если фамилия менялась
    age :: Int -- Возраст, должен быть неотрицательным
  }
  deriving (Show, Eq)

data Adult = Adult
  { personData :: Child, -- основаня информация о человеке
    idNumber :: (Int, Int) -- Номер паспорта: состоит из серии и номера.
  }
  deriving (Show, Eq)

data NewPerson
  = AdultPerson Adult
  | ChildPerson Child
  deriving (Show, Eq)

instance MyEq NewPerson where
  (===) :: NewPerson -> NewPerson -> Bool
  (===) (AdultPerson a) (AdultPerson b) = idNumber a == idNumber b
  (===) (ChildPerson a) (ChildPerson b) = firstName a == firstName b && lastName a == lastName b
  (===) _ _ = False

-- Строка должна состоять из имени, фамилии и возраста.
-- Между именем и фамилией пробел, дальше запятая, пробел, и возраст.
instance ToString Child where
  toString:: Child -> String
  toString child = firstName child ++ " " ++ lastName child ++ ", " ++ toString (age child)

instance ToString NewPerson where
  toString :: NewPerson -> String
  toString (AdultPerson person) = toString (personData person)
  toString (ChildPerson person) = toString person

-- Увеличить возраст на 1
ageUpChild :: Child -> Child
ageUpChild child = child {age = age child + 1}

ageUp :: NewPerson -> NewPerson
ageUp (AdultPerson person) =
  AdultPerson person {personData = (ageUpChild . personData) person}
ageUp (ChildPerson person)
  | age person == 13 =
    AdultPerson -- это corner case, красиво не обработать :(
      Adult
        { personData = person {age = 14},
          idNumber = (0, 0) -- паспорт пока не выдали, ничего не можем сказать про это
        }
  | otherwise = ChildPerson (ageUpChild person)

-- Сменить фамилию.
-- Если новая фамилия совпадает с текущей, ничего не меняется
-- Старая фамилия запоминается в formerLastNames

updateLastNameChild :: Child -> String -> Child
updateLastNameChild a newLastName 
  | lastName a == newLastName = a
  | otherwise =
      a
        { formerLastNames = lastName a : formerLastNames a,
          lastName = newLastName
        }
updateLastName :: NewPerson -> String -> NewPerson
updateLastName (ChildPerson a) newLastName = ChildPerson (updateLastNameChild a newLastName)
updateLastName (AdultPerson a) newLastName = AdultPerson a {personData = updateLastNameChild (personData a) newLastName}

validateBasicPerson :: Child -> Bool
validateBasicPerson person = firstName person /= [] && lastName person /= [] && age person >= 0

-- Проверки на корректность (указаны в комментариях к типу данных)
validatePerson :: NewPerson -> Bool
validatePerson (AdultPerson person) = (validateBasicPerson . personData) person && (age . personData) person >= 14
validatePerson (ChildPerson person) = validateBasicPerson person && age person < 14

-- Проверить, что два человека -- тезки.
-- Тезки -- разные люди с одинаковыми именами и фамилиями

namesakesBasicPerson :: Child -> Child -> Bool
namesakesBasicPerson a b = firstName a == firstName b && lastName a == lastName b

namesakes :: NewPerson -> NewPerson -> Bool
namesakes (AdultPerson a) (ChildPerson b) = namesakesBasicPerson (personData a) b
namesakes (AdultPerson a) (AdultPerson b) = namesakesBasicPerson (personData a) (personData b)  && idNumber a /= idNumber b
namesakes (ChildPerson a) (AdultPerson b) = namesakes (AdultPerson b) (ChildPerson a)
namesakes (ChildPerson a) (ChildPerson b) = namesakesBasicPerson a b
