module Command
  ( Command(..)
  ) where

data Command
  = Next
  | Back
  | Inspect String
  deriving (Show, Read)
