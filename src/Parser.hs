module Parser (parseLispVal) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Datatypes

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

whitespace :: Parser ()
whitespace = skipMany1 space

ident :: Parser LispVal
ident = do
  x <- letter <|> symbol
  xs <- many (letter <|> digit <|> symbol)
  return (Ident (x:xs))

number :: Parser LispVal
number = do
  ds <- many1 digit
  let n = read ds :: Int
  return (Number n)

listContents :: Parser LispVal
listContents = do
  es <- try (sepBy lispVal whitespace) <|> endBy lispVal whitespace
  return (List es)

dottedListContents :: Parser LispVal
dottedListContents = do
  e1 <- lispVal
  whitespace
  char '.'
  whitespace
  e2 <- lispVal
  spaces
  return (Pair e1 e2)

listExpr :: Parser LispVal
listExpr =
  between (char '(' >> spaces) (char ')') (try dottedListContents <|> listContents)

quoted :: Parser LispVal
quoted = do
  char '\''
  e <- lispVal
  return (List [Ident "quote", e])

lispVal :: Parser LispVal
lispVal = ident
  <|> number
  <|> quoted
  <|> listExpr

parseLispVal :: String -> Either ParseError LispVal
parseLispVal str = parse lispVal "lisp" str