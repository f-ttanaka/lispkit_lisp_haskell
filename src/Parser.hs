module Parser (parseSExpr) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Expr

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

whitespace :: Parser ()
whitespace = skipMany1 space

ident :: Parser SExpr
ident = do
  x <- letter <|> symbol
  xs <- many (letter <|> digit <|> symbol)
  return (Ident (x:xs))

number :: Parser SExpr
number = do
  ds <- many1 digit
  let n = read ds :: Int
  return (Number n)

listContents :: Parser SExpr
listContents = do
  es <- sepBy sexpr whitespace
  return (List es)

dottedListContents :: Parser SExpr
dottedListContents = do
  e1 <- sexpr
  whitespace
  char '.'
  whitespace
  e2 <- sexpr
  return (DottedList e1 e2)

listExpr :: Parser SExpr
listExpr = do
  char '('
  x <- try listContents <|> dottedListContents
  char ')'
  return x

quoted :: Parser SExpr
quoted = do
  char '\''
  e <- sexpr
  return (Quote e)

sexpr :: Parser SExpr
sexpr = ident
  <|> number
  <|> quoted
  <|> listExpr

parseSExpr :: String -> String
parseSExpr str = case parse sexpr "lisp" str of
  Left err -> show err
  Right _ -> "ok"