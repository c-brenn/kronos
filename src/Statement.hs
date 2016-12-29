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
  = Seq
      (Assign "x" (Const (I 0)))
      (While (Var "x" `Lt` Const (I 10))
        ( Seq
            ( Print  (Var "x") )
            ( Assign "x" (Add (Var "x") (Const (I 1))) )
        )
      )
