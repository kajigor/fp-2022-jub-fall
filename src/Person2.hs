{-# LANGUAGE InstanceSigs #-}
module Person2 where

import MyEq (MyEq (..))
import ToString

data IdNumber = 
    PassportNumber { series :: Int, number :: Int }
    | BirthCertificateNumber { birthCertificateNumber :: Int }
    deriving (Show, Eq)
data Person2 = Person2
    { firstName :: String         -- Имя, должно быть непустым
    , lastName :: String          -- Фамилия, должна быть непустой
    , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
    , age :: Int                  -- Возраст, должен быть неотрицательным
    , idNumber :: IdNumber
    }                             
    deriving (Show, Eq)

instance MyEq IdNumber where
    (===) (PassportNumber s1 n1) (PassportNumber s2 n2) = s1 == s2 && n1 == n2
    (===) (BirthCertificateNumber id1) (BirthCertificateNumber id2) = id1 == id2
    (===) _ _ = False

instance MyEq Person2 where
    (===) :: Person2 -> Person2 -> Bool
    (===) x y = idNumber x === idNumber y

instance ToString Person2 where
  toString :: Person2 -> String
  toString person = (firstName person) ++ " " ++ (lastName person) ++ ", " ++ (show $ age person)

ageUp :: Person2 -> Person2
ageUp person = person { age = (age person) + 1 }

updateLastName :: Person2 -> String -> Person2
updateLastName person newLastName = if newLastName /= (lastName person)
                                    then person { formerLastNames = (lastName person) : (formerLastNames person), lastName = newLastName }
                                    else person

validatePerson :: Person2 -> Bool
validatePerson person = 
    (firstName person) /= "" && (lastName person) /= "" && (age person) >= 0 && 
    case (idNumber person) of
        PassportNumber _ _ -> (age person) >= 14
        BirthCertificateNumber _ -> (age person) < 14

namesakes :: Person2 -> Person2 -> Bool
namesakes x y = (firstName x) == (firstName y) && (lastName x) == (lastName y) && x =/= y