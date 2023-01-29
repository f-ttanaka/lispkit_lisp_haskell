module Eval (runEval) where

import           Control.Monad.Trans        (lift)
import Control.Monad.Trans.Reader
    ( ReaderT(runReaderT), ask, local )
import qualified Data.Map as M
import Datatypes

type Env = M.Map String LispVal
type Eval a = ReaderT Env (Either Error) a

throwError :: Error -> Eval a
throwError err = lift (Left err)

guard :: Bool -> Error -> Eval ()
guard True _ = return ()
guard False err = throwError err

unpackIdent :: LispVal -> Eval String
unpackIdent (Ident x) = return x
unpackIdent v = throwError (TypeMismatch "identifier" v)

unpackNum :: LispVal -> Eval Int
unpackNum v = do
  res <- eval v
  case res of
    Number n -> return n
    _ -> throwError (TypeMismatch "number" v)

numericBinOp :: (Int -> Int -> Int) -> [LispVal] -> Eval LispVal
numericBinOp op es = do
  ns <- mapM unpackNum es
  return (Number (foldl1 op ns))

primitives :: [(String, [LispVal] -> Eval LispVal)]
primitives = [
  ("+", numericBinOp (+)),
  ("-", numericBinOp (-)),
  ("*", numericBinOp (*)),
  ("/", numericBinOp div)
  ]

evalSpecialForm :: String -> [LispVal] -> Eval LispVal
evalSpecialForm "quote" [v] = return v
evalSpecialForm "car" [v] = do
  res <- eval v
  case res of
    List (h:_) -> return h
    Pair v1 _ -> return v1
    _ -> throwError (TypeMismatch "list" v)
evalSpecialForm "cdr" [v] = do
  res <- eval v
  case res of
    List (_:vs) -> return (List vs)
    Pair _ v2 -> return v2
    _ -> throwError (TypeMismatch "list" v)
evalSpecialForm "cons" [v1,v2] = do
  res1 <- eval v1
  res2 <- eval v2
  case res2 of
    List vs -> return (List (res1:vs))
    _ -> return (Pair res1 res2)
evalSpecialForm "=" [v1,v2] = do
  n1 <- unpackNum v1
  n2 <- unpackNum v2
  return (Boolean (n1 == n2))
evalSpecialForm "atom" [v] = do
  res <- eval v
  case res of
    Number _ -> return (Boolean True)
    Ident _ -> return (Boolean True)
    _ -> return (Boolean False)
evalSpecialForm "if" [v1,v2,v3] = do
  res1 <- eval v1
  case res1 of
    Boolean True -> eval v2
    Boolean False -> eval v3
    _ -> throwError (TypeMismatch "boolean" v1)
evalSpecialForm "lambda" [List vs, bod] = do
  xs <- mapM unpackIdent vs
  guard (isNonoverlap xs) OverlappedVariable
  return (Lambda xs bod)
  where
    isNonoverlap :: [String] -> Bool
    isNonoverlap [] = True
    isNonoverlap (x:xs) = notElem x xs && isNonoverlap xs
evalSpecialForm "let" (e:bs) = do
  xvs <- mapM unpackLetBind bs
  local (M.union (M.fromList xvs)) (eval e)
  where
    unpackLetBind (Pair (Ident x) v) = evalLambdaBody v >>= (\vx -> return (x,vx))
    unpackLetBind v = throwError (BadSpecialForm "expected: bounded pair, actual:" v)
evalSpecialForm "letrec" (e:bs) = do
  xvs <- mapM unpackLetrecBind bs
  local (M.union (M.fromList xvs)) (eval e)
  where
    unpackLetrecBind (Pair (Ident x) v) = eval v >>= (\vx -> return (x,vx))
    unpackLetrecBind v = throwError (BadSpecialForm "expected: bounded pair, actual:" v)
evalSpecialForm f xs
  | Just p <- lookup f primitives = p xs
  | otherwise = throwError (UnboundVar "unbound function:" f)

subst :: [String] -> [LispVal] -> LispVal -> Eval LispVal
subst xs es bod = do
  guard (length xs == length es) (NumArgs (length xs) es)
  vs <- mapM eval es
  local (M.union (M.fromList (zip xs vs))) (eval bod)
 
eval :: LispVal -> Eval LispVal
eval (Number n) = return (Number n)
eval (Ident str) = do
  env <- ask
  case M.lookup str env of
    Just v -> return v
    _ -> throwError (UnboundVar "unbound variable: " str)
eval (List (Ident f : vs)) = do
  env <- ask
  case M.lookup f env of
    Just (Lambda xs bod) -> subst xs vs bod
    _ -> evalSpecialForm f vs
eval v = throwError (BadSpecialForm "bad" v)

evalLambdaBody :: LispVal -> Eval LispVal
evalLambdaBody (Lambda xs bod) = fmap (Lambda xs) (eval bod)
evalLambdaBody v = return v

runEval :: LispVal -> Either Error LispVal
runEval ex = runReaderT (eval ex) M.empty