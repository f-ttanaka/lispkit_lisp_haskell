module Datatypes where

-- values
data LispVal = Number Int
  | Ident String
  | Boolean Bool
  | List [LispVal]
  | Pair LispVal LispVal
  | Lambda [String] LispVal

instance Eq LispVal where
  Number m == Number n = m == n
  _ == _ = False

instance Show LispVal where
  show (Number n) = show n
  show (Ident s) = s
  show (Boolean b) = "#" ++ if b then "t" else "f"
  show (List vs) = "(" ++ unwords [show v | v <- vs] ++ ")"
  show (Pair v1 v2) = "(" ++ show v1 ++ " . " ++ show v2 ++ ")"
  show (Lambda {}) = "closure"

-- errors
data Error = NumArgs Int [LispVal]
  | TypeMismatch String LispVal
  | Parser String
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | OverlappedVariable

instance Show Error where
  show (UnboundVar str x) = str ++ ": " ++ x
  show (BadSpecialForm str ex) = str ++ ": " ++ show ex
  show (NotFunction str f) = str ++ ": " ++ show f
  show (NumArgs n es) = "Expected " ++ show n
                           ++ " args; found values " ++ unwords [show e | e <- es]
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show OverlappedVariable = "variable is overlapped in function definition"
