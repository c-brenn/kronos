module Main where

import Program        ( fromAST )
import Statement      ( Statement(..) )
import Interpreter    ( run, runInterpreter )
import StaticAnalysis ( unusedVariables )

import System.Environment ( getArgs )

main :: IO ()
main = do
  [filename] <- getArgs
  program <- fmap (fromAST . read) (readFile filename)
  mapM_ (\var -> putStrLn ("Warning: " ++ var ++ " is declared but not used.")) (unusedVariables program)
  result <- runInterpreter $ run program
  case result of
    Left exception ->
      putStrLn $ "Uncaught exception: " ++ exception
    Right _ ->
      putStrLn "exiting..."
