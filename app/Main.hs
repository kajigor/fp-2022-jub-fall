module Main where

import Integration (evalIntegralSimpson)


import Options.Applicative
import Expr
import Parser


main :: IO ()
main = do
    runAction =<< execParser opts
  where
    opts = info (actionParser <**> helper)
      (  fullDesc
      <> progDesc "This application evaluates definite integrals with some fixed precision."
      <> header "Simple integral calculator."
      )

data Action = Action
            { input :: String
            , a :: Double
            , b :: Double
            , eps :: Double }
            deriving (Show)

actionParser :: Parser Action
actionParser =
  Action <$> inputParser <*> aParamParser <*> bParamParser <*> epsParamParser

runAction :: Action -> IO ()
runAction (Action input a b eps) = do
    case parse input of 
      Nothing -> print ("couldn't parse expression " ++ input)
      Just expr -> case evalIntegralSimpson (eval expr) a b eps of 
        Left message -> print message
        Right res -> print res

inputParser :: Parser String
inputParser = strOption ( long "function" <> short 'f' <> metavar "STRING" <> help "Function to be integrated." )

aParamParser :: Parser Double
aParamParser = option auto ( long "from" <> short 'a' <> metavar "DOUBLE" <> help "Start of integration segment." )

bParamParser :: Parser Double
bParamParser = option auto ( long "to" <> short 'b' <> metavar "DOUBLE" <> help "End of integration segment." )

epsParamParser :: Parser Double
epsParamParser = option auto ( long "eps" <> short 'e' <> metavar "DOUBLE" <> help "Precision of integration." )
