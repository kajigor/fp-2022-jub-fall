module Test.Expr where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog

import Expr.AST
import qualified Expr.Infix as Infix
import qualified Expr.Prefix as Prefix

-- Тут мы проверяем, что парсеры для арифметических выражений написаны корректно.
-- Генерировать осмысленные входные строки просто так не получится,
-- поэтому мы генерируем выражения, дальше преобразовываем их в строку и запускаем на них парсер.
-- Это не идеально, потому что принтер печатает строки только одним способом, а пользователи
-- могут писать то же выражение сотней разных способов, но это лучше, чем ничего :)

-- Для простых типов данных генератор выбирает одно из возможных значений
genOp :: Gen Op
genOp = Gen.element [Plus, Minus, Mult, Div, Pow]

-- Чтобы написать генератор для рекурсивного алгебраического типа данных,
-- который генерирует разнообразные значения, стоит использвать Gen.recursive и Gen.subterm
genExpr :: Int -> Gen Expr
genExpr n =
  Gen.recursive
    Gen.choice
    [ -- нерекурсивные генераторы
      numGen
    ]
    [ -- рекурсивные генераторы
      binOpGen
    ]
  where
    numGen = Number <$> Gen.int (Range.constant 0 n)
    binOpGen = do
      op <- genOp
      Gen.subterm2 (genExpr n) (genExpr n) (BinOp op)

-- parser . printer == id
parserPrinterIsId :: MonadTest m => (Expr -> String) -> (String -> Maybe (String, Expr)) -> Expr -> m ()
parserPrinterIsId printer parser ast =
  case parser (printer ast) of
    Just ("", r) -> r === ast
    _ -> failure

-- Проверяем инфиксный парсер
prop_printerParserInfix :: Property
prop_printerParserInfix = property $ do
  expr <- forAll $ genExpr 100
  parserPrinterIsId printInfix Infix.parse expr

-- Проверяем префиксный парсер
prop_printerParserPrefix :: Property
prop_printerParserPrefix = property $ do
  expr <- forAll $ genExpr 100
  parserPrinterIsId printPrefix Prefix.parse expr

props :: [TestTree]
props =
  [ testProperty "`parser . printer == id` for Infix" prop_printerParserInfix
  , testProperty "`parser . printer == id` for Prefix" prop_printerParserPrefix
  ]
