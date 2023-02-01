module Datatypes where

-- expression
data Expr = Num Int
  | Ident String
  | List [Expr]
  | Pair Expr Expr

instance Show Expr where
  show (Num n) = show n
  show (Ident s) = s
  show (List es) = "(" ++ unwords [show e | e <- es] ++ ")"
  show (Pair e1 e2) = "(" ++ show e1 ++ " . " ++ show e2 ++ ")"

-- errors
data Error = NumArgs Int
  | TypeMismatch String Expr
  | Parser String
  | BadSpecialForm String Expr
  | NotFunction String String
  | UnboundVar String String
  | OverlappedVariable

instance Show Error where
  show (UnboundVar str x) = str ++ ": " ++ x
  show (BadSpecialForm str ex) = str ++ ": " ++ show ex
  show (NotFunction str f) = str ++ ": " ++ show f
  show (NumArgs n) = "Expected " ++ show n ++ " args"
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show OverlappedVariable = "variable is overlapped in function definition"
