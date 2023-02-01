module Interpreter.Eval where

import Control.Monad.Trans.Reader
    ( ReaderT(runReaderT), ask, local )
import qualified Data.Map as M
import Datatypes
import Interpreter.Datatypes

unpackIdent :: Expr -> Eval String
unpackIdent (Ident x) = return x
unpackIdent v = throwError (TypeMismatch "identifier" v)

evalQuoted :: Expr -> Val
evalQuoted (Num n) = VNum n
evalQuoted (Ident x) = VIdent x
evalQuoted (List es) = VList [evalQuoted e | e <- es]
evalQuoted (Pair e1 e2) = VPair (evalQuoted e1) (evalQuoted e2)

eval :: Expr -> Eval Val
eval (Num n) = return (VNum n)
eval (Ident x) = do
  env <- ask
  case M.lookup x env of
    Just (YetEval e) -> eval e
    Just v -> return v
    _ -> throwError (UnboundVar "unbound variable: " x)
eval (List [Ident "quote" ,e]) = return (evalQuoted e)
eval (List [Ident "car", e]) = do
  v <- eval e
  case v of
    VList (vh:_) -> return vh
    VPair v1 _ -> return v1
    _ -> throwError (TypeMismatch "list" e)
eval (List [Ident "cdr", e]) = do
  v <- eval e
  case v of
    VList (_:vs) -> return (VList vs)
    VPair _ v2 -> return v2
    _ -> throwError (TypeMismatch "list" e)
eval (List [Ident "cons", e1, e2]) = do
  v1 <- eval e1
  v2 <- eval e2
  case v2 of
    VList vs -> return (VList (v1:vs))
    _ -> return (VPair v1 v2)
eval (List [Ident "atom", e]) = do
  v <- eval e
  case v of
    VNum _ -> return (VBool True)
    VIdent _ -> return (VBool True)
    _ -> return (VBool False)
eval (List [Ident "if", e1, e2, e3]) = do
  v1 <- eval e1
  case v1 of
    VBool True -> eval e2
    VBool False -> eval e3
    _ -> throwError (TypeMismatch "boolean" e1)
eval (List [Ident "lambda", List es, e]) = do
  xs <- mapM unpackIdent es
  guard (isNonoverlap xs) OverlappedVariable
  env <- ask
  return (VClosure xs e env)
  where
    isNonoverlap :: [String] -> Bool
    isNonoverlap [] = True
    isNonoverlap (x:xs) = notElem x xs && isNonoverlap xs
eval (List (Ident "let" : e : xes)) = do
  xvs <- mapM unpackLetBind xes
  local (M.union (M.fromList xvs)) (eval e)
  where
    unpackLetBind :: Expr -> Eval (String, Val)
    unpackLetBind (Pair (Ident x) ex) = do
      v <- eval ex
      return (x,v)  
    unpackLetBind ex = throwError (BadSpecialForm "expected: bounded pair, actual:" ex)
eval (List (Ident "letrec" : e : xes)) = do
  xvs <- mapM unpackLetrecBind xes
  local (M.union (M.fromList xvs)) (eval e)
  where
    unpackLetrecBind :: Expr -> Eval (String,Val)
    unpackLetrecBind (Pair (Ident x) ex) = return (x, YetEval ex)
    unpackLetrecBind ex = throwError (BadSpecialForm "expected: bounded pair, actual:" ex)
eval ex@(List (e:es)) = do
  v <- eval e
  case v of
    VPrimitive pr -> pr es
    VClosure xs bod env -> do
      vs <- mapM eval es
      let xn = length xs in guard (xn == length vs) (NumArgs xn)
      let le = M.union (M.fromList (zip xs vs)) env
      local (const le) (eval bod)
    _ -> throwError (BadSpecialForm "bad" ex)
eval e = throwError (BadSpecialForm "cannot evaluate: " e)

runEval :: Env -> Expr -> Either Error Val
runEval env ex = runReaderT (eval ex) env


--res =
--  (letrec (f 5) (f . (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))))
--  (
--    <closure> f (f 5) fromList 
--      [
--        ("f",<closure> n (if (= n 0) 1 (* n (f (- n 1)))) 
--          fromList
--          [("*",<primitive>),("+",<primitive>),("-",<primitive>),("/",<primitive>),("=",<primitive>),
--           ("f",<closure> n (if (= n 0) 1 (* n (f (- n 1)))) fromList [("*",<primitive>),("+",<primitive>),("-",<primitive>),("/",<primitive>),("=",<primitive>)])
--          ]
--        )
--      ]
--    <closure> n (if (= n 0) 1 (* n (f (- n 1)))) fromList [("*",<primitive>),("+",<primitive>),("-",<primitive>),("/",<primitive>),("=",<primitive>)])