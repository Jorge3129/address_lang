module Main where

import AST
import Lang
import Parse
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  fileHandle <- openFile fileName ReadMode
  contents <- hGetContents fileHandle
  --   putStrLn contents
  let parsedAST = parseOrThrow parseProg contents
  result <- runProgram parsedAST ([], [])
  print result
  hClose fileHandle