module Statement ( Statement(..)) where

import Expression (Expression(..), Value(..))

data Statement
  = Assign String Expression
  | If Expression Statement Statement
  | While Expression Statement
  | Print Expression
  | Seq Statement Statement
  deriving (Show, Read)
