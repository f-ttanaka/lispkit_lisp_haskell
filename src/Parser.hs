module Parser (parseExpr) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Datatypes

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

whitespace :: Parser ()
whitespace = skipMany1 space

ident :: Parser Expr
ident = do
  x <- letter <|> symbol
  xs <- many (letter <|> digit <|> symbol)
  return (Ident (x:xs))

number :: Parser Expr
number = do
  ds <- many1 digit
  let n = read ds :: Int
  return (Num n)

listContents :: Parser Expr
listContents = do
  es <- try (sepBy lispVal whitespace) <|> endBy lispVal whitespace
  return (List es)

dottedListContents :: Parser Expr
dottedListContents = do
  e1 <- lispVal
  whitespace
  char '.'
  whitespace
  e2 <- lispVal
  spaces
  return (Pair e1 e2)

listExpr :: Parser Expr
listExpr =
  between (char '(' >> spaces) (char ')') (try dottedListContents <|> listContents)

quoted :: Parser Expr
quoted = do
  char '\''
  e <- lispVal
  return (List [Ident "quote", e])

lispVal :: Parser Expr
lispVal = ident
  <|> number
  <|> quoted
  <|> listExpr

parseExpr :: String -> Either ParseError Expr
parseExpr str = parse lispVal "lisp" str