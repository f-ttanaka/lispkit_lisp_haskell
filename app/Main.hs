module Main (main) where

import System.Environment
import Parser (parseSExpr)

main :: IO ()
main = do
  str:_ <- getArgs
  putStrLn (parseSExpr str)

