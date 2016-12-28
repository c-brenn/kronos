module Interpreter
  ( run
  , runInterpreter
  ) where

import Eval        ( runEval, eval )
import Environment ( Environment(..), new )
import Program     ( Program(..), Instruction(..), PC )

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State

data InterpreterState
  = InterpreterState
    { executed :: Program
    , env       :: Environment
    , pc        :: PC
    }

new :: InterpreterState
new =
  InterpreterState [] Environment.new 0

type Interpreter a = StateT InterpreterState (ExceptT String IO) a
runInterpreter p = runExceptT $ runStateT p Interpreter.new

run :: Program -> Interpreter ()
run program = do
  pc <- gets pc
  unless (pc >= length program) $ run' program pc

run' :: Program -> PC -> Interpreter ()
run' program currentPC = do
  liftIO $ print ("Current PC: " ++ show currentPC)
  let instruction = program !! currentPC
  liftIO $ print ("Current Instruction: " ++ show instruction)
  execute instruction
  storeExecuted instruction
  run program


execute :: Instruction -> Interpreter ()
execute _ = do
  iState <- get
  put iState { pc = pc iState + 1 }

storeExecuted :: Instruction -> Interpreter ()
storeExecuted instruction = do
  iState <- get
  put iState { executed = instruction : executed iState }
