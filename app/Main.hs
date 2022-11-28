{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Lambda
import Options.Applicative
import Parse
import Reductions
import System.FilePath ((<.>))
import Text.Megaparsec (parseMaybe)
import Text.Printf (printf)

main :: IO ()
main = do
  runAction =<< execParser opts
  where
    opts =
      info
        (actionParser <**> helper)
        ( fullDesc
            <> progDesc "This application reads lambda term from the input file and prints its reduced form in different representations."
            <> header "Lambda calculator"
        )

-- Тип данных, агрегирующий все аргументы командной строки, возвращается actionParser-ом
data Action = Action
  { input :: Input,
    output :: Output,
    strategy :: Strategy
  }
  deriving (Show)

-- Парсер аргументов командной строки
actionParser :: Options.Applicative.Parser Action
actionParser =
  Action <$> inputParser <*> outputParser <*> strategyParser

-- Основная функция приложения
runAction :: Action -> IO ()
runAction (Action input' o strategy') = do
  i <- getInput input'
  let e = parseMaybe parseLambda i
  case e of
    Nothing -> printOutput o (printf "Can't parse input: %s" i)
    Just expr -> do
      let r = eval strategy' expr
      printOutput o (outputRepresentation r)

-- Тип входных данных
data Input
  = FileInput FilePath -- Имя входного файла
  | StrInput String -- Строка, передаваемая аргументом командной строки
  deriving (Show)

-- Тип выходных данных
data Output
  = FileOutput FilePath -- Имя файла для результата
  | StandardOutput -- Вывод в консоль
  deriving (Show)

-- Парсер аргумента, специфицирующий, откуда брать входные данные
inputParser :: Options.Applicative.Parser Input
inputParser = fileInput <|> strInput

-- Флаг -i/--input позволяет задать строку -- имя входного файла
fileInput :: Options.Applicative.Parser Input
fileInput =
  FileInput
    <$> strOption --
      ( short 'i' -- короткое имя флага (-i)
          <> long "input" -- длинное имя флага (--input)
          <> metavar "INPUT" -- как аргумент этой опции называется в документации
          <> help "Input file" --
      )

-- Можно не использовать флаг i, а просто написать входную строку (1+2 в stack run exe -- 1+2)
strInput :: Options.Applicative.Parser Input
strInput = StrInput <$> strArgument (metavar "STRING" <> help "String to be parsed")

-- Парсер аргумента, специфицирующий, куда писать результат работы программы
outputParser :: Options.Applicative.Parser Output
outputParser = fileOutput <|> pure StandardOutput

-- Флаг -o/--output позволяет задать строку -- имя выходного файла
fileOutput :: Options.Applicative.Parser Output
fileOutput =
  FileOutput
    <$> strOption
      ( short 'o'
          <> long "output"
          <> metavar "OUTPUT"
          <> help (printf "Output file. If not specified, print to stdout.")
      )

-- Флаг, специфицирующий, какой из двух парсеров арифметических выражений надо применить
strategyParser :: Options.Applicative.Parser Strategy
strategyParser =
  flag
    NormalOrder
    ApplicativeOrder
    ( short 's'
        <> help "Make reductions in applicative order. If not specified, use normal order."
    )

-- Вспомогательная функция, подготавливающая входную строку -- из файла или непосредственно аргумента командной строки
getInput :: Input -> IO String
getInput (FileInput path) = readFile path
getInput (StrInput s) = return s

printOutput :: Output -> String -> IO ()
printOutput StandardOutput s = putStrLn s
printOutput (FileOutput path) s = writeFile path s

outputRepresentation :: Lambda -> String
outputRepresentation expr = printf "Named: %s\nDe Bruijn: %s\nLocally nameless: %s\n" (show expr) (show $ toDeBruijn expr) (show $ toLocallyNameless expr)

-- -- Функция, запускающая правильный парсер и записывающая результат работы в выходной файл
-- runEval :: Strategy -> Lambda -> Output -> IO ()
-- runEval s lambda out = printOutput out ()
