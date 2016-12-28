module Statement
  ( Statement(..)
  , testStatement
  ) where

import Expression (Expression(..), Value(..))

data Statement
  = Assign String Expression
  | If Expression Statement Statement
  | While Expression Statement
  | Print Expression
  | Seq Statement Statement
  deriving (Show)


testStatement :: Statement
testStatement
  = While (Var "x" `Eq` Const (B True))
    ( Seq
        ( Print (Const (I 4)) )
        ( While (Var "y" `Eq` Const (B False))
          ( Seq
              ( Print (Const (I 10)) )
              ( Print (Const (I 11)) )
          )
        )
    )
