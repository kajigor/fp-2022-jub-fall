{-# LANGUAGE InstanceSigs #-}

module Person where

import           Data.Foldable
import           Data.Function
import qualified Data.List     as List
import qualified Data.Set      as Set
import           Prelude

data Tree t
  = Leaf t
  | Node t (Set.Set (Tree t))
  deriving (Show, Eq, Ord)

data Document
  = Adult
      { passportNumber         :: (Int, Int)
      , birthCertificateNumber :: (String, String, Int)
      }
  | Minor
      { certificateNumber :: (String, String, Int) -- 2 заглавные латинские буквы, 2 заглавные буквы кирилицы, 6 цифр
      }
  deriving (Ord)

data SurnameStrategy
  = Parent1
  | Parent2
  | Combined
  deriving (Enum)

instance Show Document where
  show :: Document -> String
  show (Adult a b) = show a ++ show b
  show (Minor x)   = show x

instance Eq Document where
  (==) :: Document -> Document -> Bool
  (==) (Adult x1 y1) (Adult x2 y2) = x1 == x2 && y1 == y2
  (==) (Minor x) (Minor y) = x == y
  (==) (Adult _ xCertificateNumber) (Minor yCertificateNumber) =
    xCertificateNumber == yCertificateNumber
  (==) (Minor yCertificateNumber) (Adult _ xCertificateNumber) =
    xCertificateNumber == yCertificateNumber

-- Тип данных для человека
data Person =
  Person
    { firstName       :: String -- Имя, должно быть непустым
    , lastName        :: String -- Фамилия, должна быть непустой
    , formerLastNames :: [String] -- Предыдущие фамилии, если фамилия менялась
    , age             :: Int -- Возраст, должен быть неотрицательным
    , idNumber        :: Maybe Document -- Какое-то удостоверение личности
    , parents         :: (Maybe Person, Maybe Person) -- Родители данного человека. Выбрать подходящий контейнер.
    }
  deriving (Show, Eq, Ord)

-- Создание ребенка данных родителей
createChild ::
     Maybe Person
  -> Maybe Person
  -> String
  -> Maybe SurnameStrategy
  -> Maybe Document
  -> Person
createChild p1 p2 name strat document =
  Person
    { firstName = name
    , lastName = surnameHelper p1 p2 strat
    , formerLastNames = []
    , age = 0
    , idNumber = documentHelper document
    , parents = (p1, p2)
    }
  where
    surnameHelper ::
         Maybe Person -> Maybe Person -> Maybe SurnameStrategy -> String
    surnameHelper a b _strat =
      case (a, b) of
        (Just x, Just y) ->
          case _strat of
            Nothing       -> lastName x ++ "-" ++ lastName y
            Just Combined -> lastName x ++ "-" ++ lastName y
            Just Parent1  -> lastName x
            Just Parent2  -> lastName y
        (Just x, Nothing) -> lastName x
        (Nothing, Just y) -> lastName y
        (Nothing, Nothing) -> "Ivanov" -- Let it be default surname, 'cause why not
    documentHelper :: Maybe Document -> Maybe Document
    documentHelper doc =
      case doc of
        (Just (Adult _ _certificateNumber)) -> Just (Minor _certificateNumber)
        _                                   -> doc

-- Самый далекий предок данного человека.
-- Если на одном уровне иерархии больше одного предка -- вывести самого старшего из них.
-- Если на одном уровне иерархии больше одного предка одного максимального возраста -- вывести любого из них
emptyPerson :: Person
emptyPerson = Person "" "" [] 0 Nothing (Nothing, Nothing)

greatestAncestor :: Person -> (Person, Int)
greatestAncestor person = helper (Just person) 0
  where
    helper :: Maybe Person -> Int -> (Person, Int)
    helper Nothing _ = (emptyPerson, 0)
    helper (Just p) acc =
      case parents p of
        (Nothing, Nothing) -> (p, acc)
        (p1, p2) ->
          maximumBy
            (compare `on` (\(_person, _acc) -> (_acc, age _person)))
            [helper p1 (acc + 1), helper p2 (acc + 1)]

-- Предки на одном уровне иерархии.
ancestors :: Int -> Person -> Set.Set Person
ancestors lvl person = helper (Just person) lvl
  where
    helper :: Maybe Person -> Int -> Set.Set Person
    helper Nothing _ = Set.empty
    helper (Just _person) 0 = Set.singleton _person
    helper (Just _person) l = do
      let (p1, p2) = parents _person
      Set.union (helper p1 (l - 1)) (helper p2 (l - 1))

-- Возвращает семейное древо данного человека, описывающее его потомков.
descendants :: Set.Set Person -> Person -> Tree Person
descendants peopleSet person = do
  case length childrenSet of
    0 -> Leaf person
    _ ->
      Node
        person
        (Set.fromList (map (descendants peopleSet) (toList childrenSet)))
  where
    childrenSet = Set.filter isChild peopleSet
    isChild :: Person -> Bool
    isChild p =
      case parents p of
        (Just x, Just y)   -> person == x || person == y
        (Nothing, Just y)  -> person == y
        (Just x, Nothing)  -> person == x
        (Nothing, Nothing) -> False
