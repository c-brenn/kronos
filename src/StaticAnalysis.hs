module StaticAnalysis ( unusedVariables ) where

import Program    ( Instruction(..), Program(..) )
import Expression ( Expression(..) )

import qualified Data.Set as Set

unusedVariables :: Program -> Set.Set String
unusedVariables program = Set.difference (declaredVariables program) (usedVariables program)

declaredVariables :: Program -> Set.Set String
declaredVariables = foldr (isDeclaration) Set.empty
  where isDeclaration :: Instruction -> Set.Set String -> Set.Set String
        isDeclaration (Assign var _) vars = Set.insert var vars
        isDeclaration _ vars = vars

usedVariables :: Program -> Set.Set String
usedVariables = foldr (isUsage) Set.empty
  where isUsage :: Instruction -> Set.Set String -> Set.Set String
        isUsage (Assign _ exp) vars    = uses exp vars
        isUsage (Print exp) vars       = uses exp vars
        isUsage (GoToFalse exp _) vars = uses exp vars
        isUsage _ vars                 = vars

uses :: Expression -> Set.Set String -> Set.Set String
uses (Var name)  vars = Set.insert name vars
uses (Const _)   vars = vars
uses (Add e1 e2) vars = Set.union (uses e1 Set.empty) (uses e2 vars)
uses (Sub e1 e2) vars = Set.union (uses e1 Set.empty) (uses e2 vars)
uses (Mul e1 e2) vars = Set.union (uses e1 Set.empty) (uses e2 vars)
uses (Div e1 e2) vars = Set.union (uses e1 Set.empty) (uses e2 vars)
uses (And e1 e2) vars = Set.union (uses e1 Set.empty) (uses e2 vars)
uses (Or e1 e2)  vars = Set.union (uses e1 Set.empty) (uses e2 vars)
uses (Not e)     vars = uses e vars
uses (Eq e1 e2)  vars = Set.union (uses e1 Set.empty) (uses e2 vars)
uses (Gt e1 e2)  vars = Set.union (uses e1 Set.empty) (uses e2 vars)
uses (Lt e1 e2)  vars = Set.union (uses e1 Set.empty) (uses e2 vars)
