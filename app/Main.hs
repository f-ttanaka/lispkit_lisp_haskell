module Main (main) where

import System.Environment
import Parser (parseLispVal)
import Eval (runEval)

main :: IO ()
main = do
  str:_ <- getArgs
  case parseLispVal str of
    Left err -> print err
    Right ex -> print (runEval ex)
