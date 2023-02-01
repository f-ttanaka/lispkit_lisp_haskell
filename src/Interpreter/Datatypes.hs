module Interpreter.Datatypes where

import Datatypes
import qualified Data.Map as M (Map)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)

data Val = VNum Int
  | VIdent String
  | VBool Bool
  | VList [Val]
  | VPair Val Val
  | VPrimitive ([Expr] -> Eval Val)
  | VClosure [String] Expr Env
  | YetEval Expr

instance Eq Val where
  VNum m == VNum n = m == n
  _ == _ = False

instance Show Val where
  show (VNum n) = show n
  show (VIdent s) = s
  show (VBool True) = "#t"
  show (VBool False) = "#f"
  show (VList vs) = "(" ++ unwords [show v | v <- vs] ++ ")"
  show (VPair v1 v2) = "(" ++ show v1 ++ " . " ++ show v2 ++ ")"
  show (VPrimitive {}) = "<primitive>"
  show (VClosure xs e env) = "<closure> " ++ unwords xs ++ " " ++ show e ++ " " ++ show env
  show (YetEval e) = "yet valuated: " ++ show e

type Env = M.Map String Val
type Eval a = ReaderT Env (Either Error) a

throwError :: Error -> Eval a
throwError err = lift (Left err)

guard :: Bool -> Error -> Eval ()
guard True _ = return ()
guard False err = throwError err