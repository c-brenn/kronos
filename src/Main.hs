module Main where

import Program     ( fromAST )
import Statement   ( Statement(..) )
import Interpreter ( run, runInterpreter )

import System.Environment ( getArgs )

main :: IO ()
main = do
  [filename] <- getArgs
  statement <- fmap (read::String->Statement) (readFile filename)
  let program = fromAST statement
  result <- runInterpreter $ run program
  case result of
    Left exception ->
      print $ "Uncaught exception: " ++ exception
    Right _ ->
      print "\nfin\n"
