module Test where

import AST
import Lang
import Parse
import System.IO

fileName :: String
fileName = "../text/test1.txt"

main :: IO ()
main = do
  fileHandle <- openFile fileName ReadMode
  contents <- hGetContents fileHandle
  --   putStrLn contents
  let parsedAST = parseOrThrow parseProg contents
  print (runProgram parsedAST ([], []))
  hClose fileHandle