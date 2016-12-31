module Program where

import Expression (Expression(..))
import Statement

type PC = Int

data Instruction
  = Assign String Expression
  | Print Expression
  | GoTo PC
  | GoToFalse Expression PC
  | NoOp
  deriving (Show)

type Program = [Instruction]

isControlFlow :: Instruction -> Bool
isControlFlow (GoTo _) = True
isControlFlow (GoToFalse _ _) = True
isControlFlow _ = False

fromAST :: Statement -> Program
fromAST (Statement.Seq s1 s2) = fromAST s1 ++ fromAST s2
fromAST (Statement.If ex s1 s2) =
  let
    s1' = fromAST s1
    s2' = fromAST s2
    jumpOverS1 = length s1' + 2 -- extra 1 for added GoTo
    jumpOverS2 = length s2' + 1
  in
    GoToFalse ex jumpOverS1 :
    s1' ++
    GoTo jumpOverS2:s2'

fromAST (Statement.While ex s) =
  let
    s' = fromAST s
    internalStatements = length s'
    jumpOverS = internalStatements + 2 -- extra 1 for added GoTo
    jumpBack  = -(internalStatements + 1)
 in
    GoToFalse ex jumpOverS :
    s' ++
    [GoTo jumpBack]

fromAST (Statement.Assign s ex) = [Program.Assign s ex]
fromAST (Statement.Print ex) = [Program.Print ex]
