module Expr where

data SExpr = Number Int
  | Ident String
  | List [SExpr]
  | DottedList SExpr SExpr
  | Quote SExpr

data Builtin = Plus | Minus | Times | Div
  | Car | Cdr | Cons | Atom | Null