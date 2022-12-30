{-# LANGUAGE FlexibleInstances #-}
module Parser where

import Data.Char
import Lambda
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
    

type Parser = Parsec Void String


parseName :: Parser String
parseName = some $ (satisfy isLetter)

parseLambda :: Parser (Lambda String)
parseLambda = 
    try parseApp <|> parseAbs <|> parseVar <|> parseBrackets
    where 
        parseApp = do
            frs <- parseVar <|> parseBrackets
            space <- char ' '
            remain <- sepBy (parseVar <|> parseAbs <|> parseBrackets) (char ' ')
            return $ foldl App frs remain 
        parseVar = Var <$> parseName
        parseAbs = Abs <$> ((char 'λ' <|> char '\\') *> parseName <* (char '.')) <*> parseLambda
        parseBrackets = char '(' *> parseLambda <* char ')'
