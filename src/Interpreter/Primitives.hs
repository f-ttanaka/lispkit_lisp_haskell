module Interpreter.Primitives where

import Datatypes
import Interpreter.Datatypes
import Interpreter.Eval (eval)
import qualified Data.Map as M (fromList)
import Data.Bifunctor (second)

unpackNum :: Expr -> Eval Int
unpackNum e = do
  v <- eval e
  case v of
    VNum n -> return n
    _ -> throwError (TypeMismatch "number" e)

numericBinOp :: (Int -> Int -> Int) -> [Expr] -> Eval Val
numericBinOp op es = do
  ns <- mapM unpackNum es
  return (VNum (foldl1 op ns))

checkEq :: [Expr] -> Eval Val
checkEq [e1,e2] = do
  n1 <- unpackNum e1
  n2 <- unpackNum e2
  return (VBool (n1 == n2))
checkEq _ = throwError (NumArgs 2)

primitives :: Env
primitives = M.fromList $ map (second VPrimitive) [
  ("+", numericBinOp (+)),
  ("-", numericBinOp (-)),
  ("*", numericBinOp (*)),
  ("/", numericBinOp div),
  ("=", checkEq)
  ]