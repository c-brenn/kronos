module Eval
  ( eval
  , runEval
  ) where

import Control.Monad.Reader

import Environment ( Environment(..), lookupVar )
import Expression  ( Expression(..), Value(..) )

type Eval a = ReaderT Environment (Either String) a
runEval env ex = runReaderT ex env

evali op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                      (I i0, I i1) -> return $ I (i0 `op` i1)
                      _            -> fail "type error in arithmetic expression"

-- Boolean typed expressions

evalb op e0 e1 = do e0' <- eval e0
                    e1' <- eval e1
                    case (e0', e1') of
                      (B i0, B i1) -> return $ B (i0 `op` i1)
                      _            -> fail "type error in boolean expression"

-- Operations over integers which produce booleans

evalib op e0 e1 = do e0' <- eval e0
                     e1' <- eval e1
                     case (e0', e1') of
                       (I i0, I i1) -> return $ B (i0 `op` i1)
                       _            -> fail "type error in arithmetic expression"

-- Evaluate an expression

eval :: Expression -> Eval Value
eval (Const v) = return v
eval (Add e0 e1) = do evali (+) e0 e1
eval (Sub e0 e1) = do evali (-) e0 e1
eval (Mul e0 e1) = do evali (*) e0 e1
eval (Div e0 e1) = do evali div e0 e1
eval (And e0 e1) = do evalb (&&) e0 e1
eval (Or e0 e1) = do evalb (||) e0 e1
eval (Not e0  ) = do evalb (const not) e0 (Const (B True))
                     where not2 a _ = not a -- hack, hack
eval (Eq e0 e1) = do evalib (==) e0 e1
eval (Gt e0 e1) = do evalib (>) e0 e1
eval (Lt e0 e1) = do evalib (<) e0 e1
eval (Var s) = do env <- ask
                  case lookupVar s env of
                    Just x -> return x
                    Nothing -> fail ("Unknown variable "++s)

