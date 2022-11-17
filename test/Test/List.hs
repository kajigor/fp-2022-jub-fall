module Test.List where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Data.List (sort)
import List

-- Это пример использования Hedgehog для property-based тестирования.
-- В этом фреймворке тестирование происходит таким образом:
-- * генерируются входные данные
-- * на входных данных запускается тестируемая функция и проверяется выполнение некоторого свойства
-- * если свойство не выполняется, входные данные минимизируются таким образом,
--   чтобы тест все еще падал, но вход было легко использовать для отладки

-- Тут мы проверяем несколько функций для работы на списках.

-- Генератор целых чисел от 0 до 100.
genInt :: Gen Int
genInt = Gen.int (Range.constant 0 100)
-- Range.constant генерирует значения вне зависимости от Size.

-- Если хочется генерировать некоторые значения чаще, чем другие,
-- или чтобы генерируемые значения зависели от Size, стоит пользоваться
-- Range.linear, Range.linearFromSource, Range.exponential или другими


-- Генерация списков целых чисел с длинами от minLength до maxLength
genList :: Int -> Int -> Gen [Int]
genList minLength maxLength = Gen.list (Range.constant minLength maxLength) genInt

-- Проверяем, что все элементы списка не больше, чем максимальное
prop_maximum :: Property
prop_maximum = property $ do
  list <- forAll $ genList 1 100
  let maxValue = maximumValue list
  assert (all (<= maxValue) list)

-- Проверяем, что все элементы списка не меньше, чем минимальное
prop_minimum :: Property
prop_minimum = property $ do
  list <- forAll $ genList 1 100
  let minValue = minimumValue list
  assert (all (>= minValue) list)

-- Генерируем отсортированные списки: генерируем обычные списки и сортируем их
genSortedList :: Int -> Int -> Gen (SortedList Int)
genSortedList minLength maxLength = Sorted . sort <$> genList minLength maxLength

-- Проверяем, что minimumValue работает одинаково на отсортированных и обычных списках
prop_minimumSorted :: Property
prop_minimumSorted = property $ do
  list <- forAll $ genSortedList 1 100
  minimumValue list === minimumValue (getSorted list)

-- Проверяем, что maximumValue работает одинаково на отсортированных и обычных списках
prop_maximumSorted :: Property
prop_maximumSorted = property $ do
  list <- forAll $ genSortedList 1 100
  maximumValue list === maximumValue (getSorted list)

-- Проверяем, что дважды обернув список получаем исходный
prop_reverseList :: Property
prop_reverseList = property $ do
  list <- forAll $ genList 1 100
  reverseList (reverseList list) === list

-- Проверяем, что быстрая версия обращения списка работает так же, как эталонная
prop_fastReverseList :: Property
prop_fastReverseList = property $ do
  list <- forAll $ genList 1 100
  reverseList list === fastReverseList list

props :: [TestTree]
props =
  [ testProperty "Maximum value is not less than all elements of the list" prop_maximum
  , testProperty "Minimum value is not more than all elements of the list" prop_minimum
  , testProperty "Maximum value is not less than all elements of the sorted list" prop_maximumSorted
  , testProperty "Minimum value is not more than all elements of the sorted list" prop_minimumSorted
  , testProperty "Reversing list twice gets the input list" prop_reverseList
  , testProperty "Fast reverse gives the same " prop_fastReverseList
  ]