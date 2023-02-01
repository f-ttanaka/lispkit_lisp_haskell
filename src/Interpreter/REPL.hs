module Interpreter.REPL (runREPL) where

import System.IO ( hFlush, stdout )
import Control.Monad.Trans.State (StateT, get, evalStateT)
import Control.Monad.Trans.Class (lift)

import Parser ( parseExpr )
import Interpreter.Datatypes
import Interpreter.Eval
import Interpreter.Primitives

type REPL = StateT Env IO ()

output :: Show s => s -> REPL
output x = lift (print x)

flushStr :: String -> IO ()
flushStr str = do
  putStr str
  hFlush stdout

readPrompt :: String -> IO String
readPrompt pr = do
  flushStr pr
  getLine

readEvalPrint :: String -> REPL
readEvalPrint str = case parseExpr str of
  Left err -> output err
  Right ex -> do
    env <- get
    case runEval env ex of
      Left err -> output err
      Right v -> output v

loop :: REPL
loop = do
  str <- lift (readPrompt "LKL>>> ")
  if str == "quit" then return ()
  else do
    readEvalPrint str
    loop

runREPL :: IO ()
runREPL = evalStateT loop primitives