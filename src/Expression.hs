module Expression
  ( Expression(..)
  , Value(..)
  ) where

data Value
  = I Int
  | B Bool
  deriving (Eq, Show, Read)

data Expression
  = Const Value
  | Add Expression Expression
  | Sub Expression Expression
  | Mul Expression Expression
  | Div Expression Expression
  | And Expression Expression
  | Or Expression Expression
  | Not Expression
  | Eq Expression Expression
  | Gt Expression Expression
  | Lt Expression Expression
  | Var String
  deriving (Eq, Show, Read)
