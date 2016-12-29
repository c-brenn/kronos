module Interpreter
  ( run
  , runInterpreter
  ) where

import Command     ( Command(..) )
import Eval        ( runEval, eval )
import Environment ( Environment(..), new, assign )
import Expression  ( Expression(..), Value(..) )
import Program     ( Program(..), Instruction(..), PC )

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import System.IO ( hFlush, stdout )

data InterpreterState
  = InterpreterState
    { executed :: Program
    , env      :: Environment
    , pc       :: PC
    , command  :: Command
    }

new :: InterpreterState
new =
  InterpreterState [] Environment.new 0 Next

type Interpreter a = StateT InterpreterState (ExceptT String IO) a
runInterpreter p = runExceptT $ runStateT p Interpreter.new

prompt :: Interpreter String
prompt = liftIO $ putStr "kronos> " *> hFlush stdout *> getLine

readCommand :: Interpreter Command
readCommand = fmap read prompt

run :: Program -> Interpreter ()
run program = do
  currentCommand <- gets command
  execute currentCommand program
  nextCommand <- readCommand
  modify $ \s -> s { command = nextCommand }
  pc' <- gets pc
  unless (pc' >= length program) $ run program

execute :: Command -> Program -> Interpreter ()
execute Next program = do
  instruction <- fmap (program !!) (gets pc)
  liftIO $ print ("Executing: " ++ show instruction)
  offset <- executeInstruction instruction
  storeExecuted instruction
  modify $ \s -> s { pc = pc s + offset }

executeInstruction :: Instruction -> Interpreter Int
executeInstruction (Assign name exp) = do
  value <- evaluate exp
  modify $ \s -> s { env = Environment.assign name value (env s) }
  return 1

executeInstruction (Print exp) = do
  value <- evaluate exp
  liftIO $ print value
  return 1

executeInstruction (GoTo offset) = return offset

executeInstruction (GoToFalse exp offset) = do
  B value <- evaluate exp
  if value
     then return 1
     else return offset

evaluate :: Expression -> Interpreter Value
evaluate exp = do
  env <- gets env
  Right value <- return $ runEval env (eval exp)
  return value

storeExecuted :: Instruction -> Interpreter ()
storeExecuted instruction = do
  modify $ \s -> s { executed = instruction : executed s }
