{-# LANGUAGE TupleSections #-}

module Expr where

import Expr.Parser
import Expr.AST

import Options.Applicative
import System.FilePath ((<.>))
import Text.Printf (printf)
import Password
import qualified Data.Map as Map
import qualified Data.Set as Set
import Expr.Lexer
import Data.Maybe (fromJust)
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Class
import qualified Expr.Combinators as P

-- Программа парсит аргументы командной строки при помощи execParser,
-- а потом запускает функцию runAction (логику приложения)
main :: IO ()
main = do
    runAction =<< execParser opts
  where
    -- Задает парсер аргументов actionParser, сопровождая его автоматической генерацией странички help.
    opts = info (actionParser <**> helper)
      (  fullDesc
      <> progDesc "This application demonstrates two parsers for arithmetic expressions: one for the infix and the other for the prefix notation."
      <> header "Simple parser console application"
      )

-- Тип данных, агрегирующий все аргументы командной строки, возвращается actionParser-ом
data Action = Action
            { input :: Input
            , output :: Output
            , dumpToStdout :: Bool
            , parserType :: ParserType }
            deriving (Show)

-- Парсер аргументов командной строки
actionParser :: Parser Action
actionParser =
  Action <$> inputParser <*> outputParser <*> dumpToStdoutParser <*> parserTypeParser

-- Основная функция приложения
runAction :: Action -> IO ()
runAction (Action input output dump pType) = do
  i <- getInput input         -- подготавливаем входные данные
  o <- getOutput output input -- подготавливаем файл для результат
  runParser pType i o         -- запускаем парсер подходящего типа
  dumpIntoStdout dump i o     -- если стоит соответствующий флаг, выводим результат работы приложения в консоль

-- Тип входных данных
data Input = FileInput FilePath -- Имя входного файла
           | StrInput String    -- Строка, передаваемая аргументом командной строки
           deriving (Show)

-- Тип выходных данных
data Output = FileOutput FilePath -- Имя файла для результата
            | DefaultOutput       -- Дефолтное имя файла
            deriving (Show)

-- Парсер аргумента, специфицирующий, откуда брать входные данные
inputParser :: Parser Input
inputParser = fileInput <|> strInput

-- Флаг -i/--input позволяет задать строку -- имя входного файла
fileInput :: Parser Input
fileInput = FileInput <$> strOption --
  (  short 'i'           -- короткое имя флага (-i)
  <> long "input"        -- длинное имя флага (--input)
  <> metavar "INPUT"     -- как аргумент этой опции называется в документации
  <> help "Input file" ) --

-- Можно не использовать флаг i, а просто написать входную строку (1+2 в stack run exe -- 1+2)
strInput :: Parser Input
strInput = StrInput <$> strArgument (metavar "STRING" <> help "String to be parsed")

-- Парсер аргумента, специфицирующий, куда писать результат работы программы
outputParser :: Parser Output
outputParser = fileOutput <|> defaultOutput

-- Флаг -o/--output позволяет задать строку -- имя выходного файла
fileOutput :: Parser Output
fileOutput = FileOutput <$> strOption
  (  short 'o'
  <> long "output"
  <> metavar "OUTPUT"
  <> help (printf "Output file. If not specified, output is INPUT.out. If INPUT is not specified, output is %s" defaultOutFile)
  )

-- Если флаг -o не использован, выбираем дефолтное имя выходного файла
defaultOutput :: Parser Output
defaultOutput = pure DefaultOutput

-- Флаг, специфицирующий, надо ли печатать результат работы приложения в консоль
dumpToStdoutParser :: Parser Bool
dumpToStdoutParser = switch
  (  short 'd'
  <> help "Render input and output into stdout"
  )

-- Флаг, специфицирующий, какой из двух парсеров арифметических выражений надо применить
parserTypeParser :: Parser ParserType
parserTypeParser = flag Infix Prefix
  (  short 'p'
  <> help "Use the parser for prefix binops. If not specified, use the parser for infix binops"
  )

-- Вспомогательная функция, подготавливающая входную строку -- из файла или непосредственно аргумента командной строки
getInput :: Input -> IO String
getInput (FileInput path) = readFile path
getInput (StrInput str) = return str

-- Вспомогательная функция, подготавливающая имя файла для результата
getOutput :: Output -> Input -> IO FilePath
getOutput (FileOutput path) _ = return path -- если путь указан, берем его
getOutput _ (FileInput path) = return $ path <.> "out" -- иначе, если вход был из файла, добавляем к его имени out
getOutput _ _ = return defaultOutFile -- иначе берем дефолтное имя файла (bad style, не делайте так)

-- Очень плохо, не надо так
defaultOutFile :: String
defaultOutFile = "str.out"

-- Функция, запускающая правильный парсер и записывающая результат работы в выходной файл
runParser :: ParserType -> String -> FilePath -> IO ()
runParser pType s path =
  case parse pType s of
    Just expr -> do
      let unboundVars = vars expr
      varMap <- runMaybeT $ askVars unboundVars
      let result = runEval expr (fromJust varMap)
      writeFile path (printf "Expr:\n%s\n\nValue:\n%d\n" (show expr) result)
    Nothing ->
      writeFile path "Syntax error"

askVars :: Set.Set String -> MaybeT IO (Map.Map String Int)
askVars vars = do
    r <- mapM (\v -> msum $ repeat (go v)) $ Set.toList vars
    return $ Map.fromList r
  where
    go :: String -> MaybeT IO (String,Int)
    go var = do
      lift $ putStrLn $ printf "Please input integer value of %s" var
      s <- lift getLine
      value <- snd <$> hoistMaybe (P.runParser (num <* P.eof) s)
      return $ (var, value)

-- Функция, которая выводит результат работы программы в консоль
dumpIntoStdout :: Bool -> String -> FilePath -> IO ()
dumpIntoStdout False _ _ = return ()
dumpIntoStdout True i o = do
  out <- readFile o
  putStrLn $ printf "===================================\nInput:\n\n%s\n-----------------------------------\nOutput:\n\n%s\n===================================\n" i out
