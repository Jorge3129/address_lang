module Main where

import AST
import Lang
import Parse
import System.Environment
import System.IO

runFile :: FilePath -> IO ()
runFile fileName = do
  progTree <- parseOrThrow parseProg <$> readFile fileName
  result <- runProgram progTree ([], [])
  print result

runBaseFile :: String -> IO ()
runBaseFile fileName = runFile ("../text/" ++ fileName ++ ".txt")

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  runFile fileName