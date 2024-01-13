module Main where

import Run
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let fileName = head args
  res <- runFile fileName
  print res
  return ()