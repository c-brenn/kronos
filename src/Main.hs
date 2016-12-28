module Main where

import Program     ( fromAST )
import Statement   ( testStatement )
import Interpreter ( run, runInterpreter )

main :: IO ()
main = do
  let program = fromAST testStatement
  result <- runInterpreter $ run program
  case result of
    Left exception ->
      print $ "Uncaught exception: " ++ exception
    Right _ ->
      print "\nfin\n"
